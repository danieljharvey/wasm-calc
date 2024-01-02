{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Calc.Linearity (validateFunction, validateModule, getFunctionUses, Linearity (..), LinearityError (..),
    LinearIdentifier(..)) where
import qualified Error.Diagnose            as Diag
import qualified Prettyprinter             as PP
import qualified Prettyprinter.Render.Text as PP


import           Calc.ExprUtils
import           Calc.Types.Annotation
import           Calc.Types.Expr
import           Calc.Types.Function
import           Calc.Types.Identifier
import           Calc.Types.Module
import           Calc.Types.Type
import           Control.Monad.State
import           Data.Foldable             (traverse_)
import qualified Data.List                 as List
import qualified Data.Map                  as M
import           Data.Maybe                (isJust)
import qualified Data.Text                 as T
import           GHC.Natural

-- | Primitive values can be used as much as you like, don't care
data Linearity
  = Primitive
  | Boxed Natural
  | Slice Natural Natural
  deriving stock (Eq, Ord, Show)

instance Semigroup Linearity where
  (Boxed a) <> (Boxed b) = Boxed (a + b)
  _ <> _                 = Primitive

instance Monoid Linearity where
  mempty = Boxed 0

newtype LinearState = LinearState {lsUses :: M.Map LinearIdentifier Linearity}

-- | difference between things and slices of them
data LinearIdentifier
  = Entire Identifier
  | Partial Natural Identifier
  deriving stock (Eq,Ord,Show)

data LinearityError
  = NotUsed Identifier
  | UsedMultipleTimes Identifier
  | SliceUsedMultipleTimes Identifier Natural
  deriving stock (Eq, Ord, Show)

positionFromAnnotation ::
  String ->
  T.Text ->
  Annotation ->
  Maybe Diag.Position
positionFromAnnotation path input ann =
  let toPos ss =
        Diag.Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
   in toPos <$> sourceSpan input ann

prettyPrint :: PP.Doc doc -> T.Text
prettyPrint = renderWithWidth 40

typeErrorDiagnostic ::
  T.Text ->
  LinearityError ->
  Diag.Diagnostic T.Text
typeErrorDiagnostic input e =
  let filename = "<repl>"
      diag = Diag.addFile mempty filename (T.unpack input)
      report = case e of
        (NotUsed ident) ->
          Diag.Err
            Nothing
            ( prettyPrint "Function type expected but not found."
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getOuterTypeAnnotation ty)
                    <*> pure
                      ( Diag.This
                          ( prettyPrint $
                              "This has type "
                                <> PP.pretty ty
                                <> "."
                          )
                      )
                ]
            )
            []
        (FunctionArgumentLengthMismatch ann expected actual) ->
          Diag.Err
            Nothing
            ( prettyPrint "Wrong number of arguments passed to function!"
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This
                          ( prettyPrint $
                              "Expected "
                                <> PP.pretty expected
                                <> " but found "
                                <> PP.pretty actual
                                <> "."
                          )
                      )
                ]
            )
            []
        (PredicateIsNotBoolean _ foundType) ->
          Diag.Err
            Nothing
            ( prettyPrint $
                "Predicate for an if statement should be a Boolean type, but instead found "
                  <> PP.pretty foundType
                  <> "."
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getOuterTypeAnnotation foundType)
                    <*> pure
                      ( Diag.This (prettyPrint $ "This has type " <> PP.pretty foundType <> " but should have type Boolean")
                      )
                ]
            )
            []
        (TypeMismatch a b) ->
          Diag.Err
            Nothing
            ( prettyPrint $
                "Unification error! Expected matching types but found "
                  <> PP.pretty a
                  <> " and "
                  <> PP.pretty b
                  <> "."
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getOuterTypeAnnotation a)
                    <*> pure
                      ( Diag.This (prettyPrint $ "This has type " <> PP.pretty a)
                      ),
                  (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getOuterTypeAnnotation b)
                    <*> pure (Diag.Where (prettyPrint $ "This has type " <> PP.pretty b))
                ]
            )
            ["These two values should be of the same type"]
        (InfixTypeMismatch _op pairs) ->
          let makeThis (expect, actual) =
                (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    (getOuterTypeAnnotation actual)
                  <*> pure
                    ( Diag.This (prettyPrint $ "This has type " <> PP.pretty actual <> " but should have type " <> PP.pretty expect)
                    )
           in Diag.Err
                Nothing
                "Type mismatch for infix operator"
                ( mapMaybe makeThis pairs
                )
                []
        (AccessingNonTuple ann ty) ->
          Diag.Err
            Nothing
            "Accessing non-tuple"
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This (prettyPrint $ "Expected a tuple type here but found " <> PP.pretty ty)
                      )
                ]
            )
            []
        (AccessingOutsideTupleBounds ann ty index) ->
          Diag.Err
            Nothing
            "Accessing item outside tuple"
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This (prettyPrint $ "Index " <> PP.pretty index <> " cannot be found in tuple " <> PP.pretty ty)
                      )
                ]
            )
            []
        (VarNotFound ann identifier existing) ->
          Diag.Err
            Nothing
            "Variable not found!"
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This (prettyPrint $ "Could not find identifier " <> PP.pretty identifier)
                      )
                ]
            )
            [Diag.Note $ "Available in scope: " <> prettyPrint (prettyHashset existing)]
        (FunctionNotFound ann fnName existing) ->
          Diag.Err
            Nothing
            "Function not found!"
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This (prettyPrint $ "Could not find function " <> PP.pretty fnName)
                      )
                ]
            )
            [Diag.Note $ "Available in scope: " <> prettyPrint (prettyHashset existing)]
        (NonBoxedGenericValue ann ty) ->
          Diag.Err
            Nothing
            "Cannot pass non-boxed value to generic function!"
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This (prettyPrint $ "Expected boxed type, instead found " <> PP.pretty ty)
                      )
                ]
            )
            [Diag.Note "Perhaps try wrapping the value in Box()"]
   in Diag.addReport diag report

-- | becomes "a, b, c, d"
prettyHashset :: (PP.Pretty a) => HashSet a -> PP.Doc ann
prettyHashset hs =
  PP.concatWith
    (PP.surround PP.comma)
    (PP.pretty <$> HS.toList hs)

renderWithWidth :: Int -> PP.Doc ann -> Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}













validateModule :: Module (Type ann) -> Either LinearityError ()
validateModule (Module {mdFunctions, mdExpr}) =
  do
    traverse_ validateFunction mdFunctions
    validate (getExprUses mdExpr)

getExprUses :: Expr (Type ann) -> M.Map LinearIdentifier Linearity
getExprUses expr =
  lsUses $
    execState
      (decorateWithUses expr)
      (LinearState {lsUses = mempty})

validateFunction :: Function (Type ann) -> Either LinearityError ()
validateFunction =
  validate . getFunctionUses

validate :: M.Map LinearIdentifier Linearity -> Either LinearityError ()
validate uses =
  let validateFunctionItem (linearIdent, linearity) =
        case linearIdent of
          Partial index ident ->
            case linearity of
              Boxed 0         -> Left (NotUsed ident)
              Boxed i | i > 1 -> Left (SliceUsedMultipleTimes ident index)
              _               -> Right ()

          Entire ident ->
            case linearity of
              Boxed 0         -> if hasPartial (M.keys uses) ident then Right () else
                                  Left (NotUsed ident)
              Boxed i | i > 1 -> Left (UsedMultipleTimes ident)
              _               -> Right ()
   in traverse_ validateFunctionItem (M.toList uses)

hasPartial :: [LinearIdentifier] -> Identifier -> Bool
hasPartial linIdents ident
  = isJust $ List.find (\case
      Partial _ pIdent -> pIdent == ident
      _ -> False) linIdents

getFunctionUses :: Function (Type ann) -> M.Map LinearIdentifier Linearity
getFunctionUses (Function {fnBody, fnArgs}) =
  lsUses $
    execState
      (decorateWithUses fnBody)
      (LinearState {lsUses = initialUses})
  where
    initialUses =
      foldMap
        ( \(ArgumentName arg, ty) ->
            M.singleton (Entire (Identifier arg)) $ case ty of
              TPrim {} -> Primitive
              _        -> Boxed 0
        )
        fnArgs

recordUse :: (MonadState LinearState m) => Identifier -> m ()
recordUse ident =
  modify (\ls -> ls {lsUses = M.insertWith mappend (Entire ident) (Boxed 1) (lsUses ls)})

recordContainerAccessUse :: (MonadState LinearState m) => Natural -> Identifier -> m ()
recordContainerAccessUse index ident =
  modify (\ls -> ls {lsUses = M.insertWith mappend
              (Partial index ident) (Boxed 1) (lsUses ls)})

addLetBinding :: (MonadState LinearState m) => Identifier -> Type ann -> m ()
addLetBinding ident ty =
  let initialLinearity = case ty of
        TPrim {} -> Primitive
        _        -> Boxed 0
   in modify (\ls -> ls {lsUses =
        M.insertWith mappend (Entire ident) initialLinearity (lsUses ls)})

decorateWithUses :: (MonadState LinearState m) =>
    Expr (Type ann) -> m (Expr (Type ann))
decorateWithUses (EVar ann ident) = do
  recordUse ident
  pure (EVar ann ident)
decorateWithUses (EContainerAccess ann (EVar ann' ident) index)
  = do
    recordContainerAccessUse index ident
    pure (EContainerAccess ann (EVar ann' ident) index)
decorateWithUses (ELet ann ident expr rest) = do
  addLetBinding ident (getOuterAnnotation expr)
  ELet ann ident <$> decorateWithUses expr <*> decorateWithUses rest
decorateWithUses other =
  bindExpr decorateWithUses other
