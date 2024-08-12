{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Typecheck.Error.TypeError (TypeError (..), typeErrorDiagnostic) where

import Calc.ExprUtils
import Calc.SourceSpan
import Calc.TypeUtils
import Calc.Typecheck.Error.PatternMatchError
import Calc.Types
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.List as List
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import GHC.Natural
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

data TypeError ann
  = PredicateIsNotBoolean ann (Type ann)
  | InfixTypeMismatch Op (Type ann) (Type ann)
  | TypeMismatch (Type ann) (Type ann)
  | VarNotFound ann Identifier (HashSet Identifier)
  | FunctionNotFound ann FunctionName (HashSet FunctionName)
  | FunctionArgumentLengthMismatch ann Int Int -- expected, actual
  | NonFunctionTypeFound ann (Type ann)
  | AccessingNonTuple ann (Type ann)
  | AccessingOutsideTupleBounds ann (Type ann) Natural
  | NonBoxedGenericValue ann (Type ann)
  | PatternMismatch (Type ann) (Pattern ann)
  | CantBindVoidValue (Pattern ann)
  | ExpectedInteger ann TypePrim
  | ExpectedFloat ann TypePrim
  | UnknownIntegerLiteral ann
  | UnknownFloatLiteral ann
  | ManualMemoryAccessOutsideLimit ann Natural Natural -- limit, value
  | CantSetConstant ann Identifier
  | ConstructorNotFound ann Constructor
  | UnknownGenericInConstructor ann Constructor TypeVar
  | PatternMatchError (PatternMatchError ann)
  deriving stock (Eq, Ord, Show)

positionFromAnnotation ::
  String ->
  Text ->
  Annotation ->
  Maybe Diag.Position
positionFromAnnotation path input ann =
  let toPos ss =
        Diag.Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
   in toPos <$> sourceSpan input ann

prettyPrint :: PP.Doc doc -> Text
prettyPrint = renderWithWidth 60

typeErrorDiagnostic ::
  Text ->
  TypeError Annotation ->
  Diag.Diagnostic Text
typeErrorDiagnostic input e =
  let filename = "<repl>"
      diag = Diag.addFile mempty filename (T.unpack input)
   in case e of
        (PatternMatchError patternMatchError) ->
          patternMatchErrorDiagnostic input patternMatchError
        (ExpectedInteger ann tyPrim) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              ( prettyPrint "Expected an integer"
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
                                "Instead got " <> PP.pretty tyPrim
                            )
                        )
                  ]
              )
              []
        (ExpectedFloat ann tyPrim) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              ( prettyPrint "Expected a float"
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
                                "Instead got " <> PP.pretty tyPrim
                            )
                        )
                  ]
              )
              []
        (UnknownGenericInConstructor ann constructor var) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              ( prettyPrint $ "Constructor " <> PP.pretty constructor <> " does not provide a type for var" <> PP.pretty var
              )
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.This
                            ( prettyPrint
                                "Perhaps add a type annotation so that we know what should go here?"
                            )
                        )
                  ]
              )
              []
        (CantSetConstant ann ident) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              ( prettyPrint $ "Cannot mutate constant " <> PP.pretty ident
              )
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.This
                            ( prettyPrint
                                "Perhaps declare this with 'global mut' instead?"
                            )
                        )
                  ]
              )
              []
        (ConstructorNotFound ann constructor) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              ( prettyPrint "Constructor could not be found."
              )
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.This
                            ( prettyPrint $ PP.pretty constructor
                            )
                        )
                  ]
              )
              []
        (UnknownFloatLiteral ann) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              ( prettyPrint "Unknown float literal."
              )
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.This
                            ( prettyPrint
                                "This could be an Float32 or an Float64."
                            )
                        )
                  ]
              )
              []
        (UnknownIntegerLiteral ann) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              ( prettyPrint "Unknown integer literal."
              )
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.This
                            ( prettyPrint
                                "This could be an Int32 or an Int64."
                            )
                        )
                  ]
              )
              []
        (NonFunctionTypeFound _ ty) ->
          Diag.addReport diag $
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
        (PatternMismatch ty pat) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              ( prettyPrint "Pattern and type do not match!"
              )
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        (getOuterPatternAnnotation pat)
                      <*> pure
                        ( Diag.This
                            ( prettyPrint $
                                "Expected this to have type "
                                  <> PP.pretty ty
                                  <> "."
                            )
                        )
                  ]
              )
              []
        (FunctionArgumentLengthMismatch ann expected actual) ->
          Diag.addReport diag $
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
          Diag.addReport diag $
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
          Diag.addReport diag $
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
        (InfixTypeMismatch _op a b) ->
          let makeThis (expect, actual) =
                (,)
                  <$> positionFromAnnotation
                    filename
                    input
                    (getOuterTypeAnnotation actual)
                  <*> pure
                    ( Diag.This (prettyPrint $ "This has type " <> PP.pretty actual <> " but should have type " <> PP.pretty expect)
                    )
           in Diag.addReport diag $
                Diag.Err
                  Nothing
                  "Type mismatch for infix operator"
                  ( List.nub (mapMaybe makeThis [(a, b)])
                  )
                  []
        (AccessingNonTuple ann ty) ->
          Diag.addReport diag $
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
          Diag.addReport diag $
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
          Diag.addReport diag $
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
          Diag.addReport diag $
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
          Diag.addReport diag $
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
        (CantBindVoidValue pat) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              "Cannot bind a void value to a variable"
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        (getOuterPatternAnnotation pat)
                      <*> pure
                        ( Diag.This (prettyPrint "Instead bind this to `_`?")
                        )
                  ]
              )
              [Diag.Note "Perhaps try wrapping the value in Box()"]
        (ManualMemoryAccessOutsideLimit ann 0 _) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              "Manual memory access is not configured."
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.This (prettyPrint "Cannot manually access memory without configuring a limit first.")
                        )
                  ]
              )
              [Diag.Note "Try adding a memory limit by adding \"memory 1000\" at the top of the module."]
        (ManualMemoryAccessOutsideLimit ann limit value) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              "Attempted to manually access memory outside the specified limit."
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.This (prettyPrint $ "Value of " <> PP.pretty value <> " should be less than the currently set limit of " <> PP.pretty limit <> ".")
                        )
                  ]
              )
              [Diag.Note $ "Perhaps increase the limit by adding \"memory " <> T.pack (show (limit + 1)) <> "\""]

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
