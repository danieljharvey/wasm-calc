{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module Calc.Linearity.Validate
  ( validateFunction,
    validateGlobal,
    validateModule,
    getFunctionUses,
    Drops (..),
  )
where

import           Calc.ExprUtils
import           Calc.Linearity.Error
import           Calc.Linearity.Types
import           Calc.Types.Expr
import           Calc.Types.Function
import           Calc.Types.Global
import           Calc.Types.Identifier
import           Calc.Types.Module
import           Calc.Types.Pattern
import           Calc.Types.Type
import           Calc.TypeUtils
import           Calc.Utils
import           Control.Monad          (unless)
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bifunctor         (second)
import           Data.Foldable          (traverse_)
import           Data.Functor           (($>))
import qualified Data.List.NonEmpty     as NE
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import           GHC.Natural

data Drops
  = DropIdentifiers (NE.NonEmpty Identifier)
  | DropMe
  deriving stock (Eq, Ord, Show)

getFresh :: MonadState (LinearState ann) m => m Natural
getFresh = do
  modify (\ls -> ls { lsFresh = lsFresh ls + 1 })
  gets lsFresh

getLinearityAnnotation :: Linearity ann -> ann
getLinearityAnnotation (Whole ann) = ann

validateModule :: (Show ann) => Module (Type ann) -> Either (LinearityError ann) ()
validateModule (Module {mdFunctions, mdGlobals}) = do
  traverse_ validateFunction mdFunctions
  traverse_ validateGlobal mdGlobals

validateGlobal ::
  (Show ann) =>
  Global (Type ann) ->
  Either (LinearityError ann) (Expr (Type ann, Maybe Drops))
validateGlobal glob =
  let (expr, linearState) = getGlobalUses glob
   in validate linearState $> expr

validateFunction ::
  (Show ann) =>
  Function (Type ann) ->
  Either (LinearityError ann) (Expr (Type ann, Maybe Drops))
validateFunction fn =
  let (expr, linearState) = getFunctionUses fn
   in validate linearState $> expr

validate :: LinearState ann -> Either (LinearityError ann) ()
validate (LinearState {lsVars, lsUses}) =
  let validateFunctionItem (ident, (linearity, _ann)) =
        let completeUses = filterCompleteUses lsUses ident
         in case linearity of
              LTPrimitive -> Right ()
                {-                if null completeUses
                  then Left (NotUsed ann ident)
                  else Right () -}
              LTBoxed ->
                case length completeUses of
                  0 -> Right ()
                    -- Left (NotUsed ann ident)
                  1 -> Right ()
                  _more ->
                    Left (UsedMultipleTimes (getLinearityAnnotation <$> completeUses) ident)
   in traverse_ validateFunctionItem (M.toList lsVars)

-- | count uses of a given identifier
filterCompleteUses ::
  [(Identifier, Linearity ann)] ->
  Identifier ->
  [Linearity ann]
filterCompleteUses uses ident =
  foldr
    ( \(thisIdent, linearity) total -> case linearity of
        Whole _ ->
          if thisIdent == ident then linearity : total else total
    )
    []
    uses

getFunctionUses ::
  (Show ann) =>
  Function (Type ann) ->
  (Expr (Type ann, Maybe Drops), LinearState ann)
getFunctionUses (Function {fnBody, fnArgs}) =
  fst $ runIdentity $ runWriterT $ runStateT action initialState
  where
    action = decorate fnBody

    initialState =
      LinearState
        { lsVars = initialVars,
          lsUses = mempty,
          lsFresh = 0
        }

    initialVars =
      foldMap
        ( \(FunctionArg {faAnn, faName = ArgumentName arg, faType}) ->
            M.singleton (Identifier arg) $ case faType of
              TPrim {} -> (LTPrimitive, getOuterTypeAnnotation faAnn)
              _        -> (LTBoxed, getOuterTypeAnnotation faAnn)
        )
        fnArgs

getGlobalUses ::
  (Show ann) =>
  Global (Type ann) ->
  (Expr (Type ann, Maybe Drops), LinearState ann)
getGlobalUses (Global {glbExpr}) =
  fst $ runIdentity $ runWriterT $ runStateT action initialState
  where
    action = decorate glbExpr

    initialState =
      LinearState
        { lsVars = mempty,
          lsUses = mempty,
          lsFresh = 0
        }

recordUse ::
  ( MonadState (LinearState ann) m,
    MonadWriter (S.Set Identifier) m
  ) =>
  Identifier ->
  Type ann ->
  m ()
recordUse ident ty = do
  modify (\ls -> ls {lsUses = (ident, Whole (getOuterTypeAnnotation ty)) : lsUses ls})
  unless (isPrimitive ty) $ tell (S.singleton ident) -- we only want to track use of non-primitive types

isPrimitive :: Type ann -> Bool
isPrimitive (TPrim {}) = True
isPrimitive _          = False

addLetBinding ::
  (MonadState (LinearState ann) m) =>
  Pattern (Type ann) ->
  m (Pattern (Type ann, Maybe Drops))
addLetBinding (PVar ty ident) = do
  modify
    ( \ls ->
        ls
          { lsVars =
              M.insert
                ident
                ( if isPrimitive ty then LTPrimitive else LTBoxed,
                  getOuterTypeAnnotation ty
                )
                (lsVars ls)
          }
    )
  pure $ PVar (ty, Nothing) ident
addLetBinding (PWildcard ty) = do
  i <- getFresh
  let name = Identifier $ "_fresh_name" <> T.pack (show i)
  addLetBinding $ PVar ty  name
addLetBinding (PBox ty pat) =
  PBox (ty, Nothing) <$> addLetBinding pat
addLetBinding (PTuple ty p ps) = do
  PTuple (ty, Nothing)
    <$> addLetBinding p
    <*> traverse addLetBinding ps

-- given an expr, throw everything inside in the bin
dropThemAll ::
  Pattern (Type ann) ->
  Expr (Type ann, Maybe Drops) ->
  Expr (Type ann, Maybe Drops)
dropThemAll (PBox _ pItem) (EBox (ty, _) item) =
  EBox (ty, Just DropMe) (dropThemAll pItem item)
dropThemAll (PTuple _ pA pAs) (ETuple (ty, _) a as) =
  ETuple (ty, Just DropMe) (dropThemAll pA a) (neZipWith dropThemAll pAs as)
dropThemAll (PWildcard _) expr =
  reallyDropThemAll expr
dropThemAll pat other =
  mapExpr (dropThemAll pat) other

-- | this should be replace with a type-generated drop function
-- that we could also pass into polymorphic functions
reallyDropThemAll ::
  Expr (Type ann, Maybe Drops) ->
  Expr (Type ann, Maybe Drops)
reallyDropThemAll (EBox (ty, _) item) =
  EBox (ty, Just DropMe) (reallyDropThemAll item)
reallyDropThemAll (ETuple (ty, _) a as) =
  ETuple (ty, Just DropMe) (reallyDropThemAll a) (reallyDropThemAll <$> as)
reallyDropThemAll e@(EApply {}) = e
reallyDropThemAll other = mapExpr reallyDropThemAll other

decorate ::
  (Show ann) =>
  (MonadState (LinearState ann) m, MonadWriter (S.Set Identifier) m) =>
  Expr (Type ann) ->
  m (Expr (Type ann, Maybe Drops))
decorate (EVar ty ident) = do
  recordUse ident ty
  pure (EVar (ty, Nothing) ident)
decorate (ELet ty pat expr rest) = do
  -- get all idents mentioned in `expr`
  (decoratedExpr, exprIdents) <- runWriterT (decorate expr)

  let drops = DropIdentifiers <$> NE.nonEmpty (S.toList exprIdents)

  ELet (ty, drops)
    <$> addLetBinding pat
    <*> pure (dropThemAll pat decoratedExpr)
    <*> (tell exprIdents >> decorate rest) -- keep hold of the stuff we learned
decorate (EPrim ty prim) =
  pure $ EPrim (ty, Nothing) prim
decorate (EInfix ty op a b) =
  EInfix (ty, Nothing) op <$> decorate a <*> decorate b
decorate (EIf ty predExpr thenExpr elseExpr) = do
  (decoratedThen, thenIdents) <- runWriterT (decorate thenExpr)
  (decoratedElse, elseIdents) <- runWriterT (decorate elseExpr)

  -- work out idents used in the other branch but not this one
  let uniqueToThen = DropIdentifiers <$> NE.nonEmpty (S.toList (S.difference thenIdents elseIdents))
      uniqueToElse = DropIdentifiers <$> NE.nonEmpty (S.toList (S.difference elseIdents thenIdents))

  EIf (ty, Nothing)
    <$> decorate predExpr
    <*> pure (mapOuterExprAnnotation (second (const uniqueToElse)) decoratedThen)
    <*> pure (mapOuterExprAnnotation (second (const uniqueToThen)) decoratedElse)
decorate (EApply ty fnName args) =
  EApply (ty, Nothing) fnName <$> traverse decorate args
decorate (ETuple ty a as) =
  ETuple (ty, Nothing) <$> decorate a <*> traverse decorate as
decorate (EBox ty a) =
  EBox (ty, Nothing) <$> decorate a
decorate (EAnn ty tyAnn a) =
  EAnn (ty, Nothing) ((,Nothing) <$> tyAnn) <$> decorate a
decorate (ELoad ty a) =
  ELoad (ty, Nothing) <$> decorate a
decorate (EStore ty a b) =
  EStore (ty, Nothing) <$> decorate a <*> decorate b
decorate (ESet ty identifier b) =
  ESet (ty, Nothing) identifier <$> decorate b
decorate (EBlock ty items) =
  EBlock (ty, Nothing) <$> decorate items
