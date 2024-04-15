{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Calc.Linearity.Validate
  ( validateFunction,
    validateModule,
    getFunctionUses,
    Drop (..),
  )
where

import           Calc.ExprUtils
import           Calc.Linearity.Error
import           Calc.Linearity.Types
import           Calc.Types.Expr
import           Calc.Types.Function
import           Calc.Types.Identifier
import           Calc.Types.Module
import           Calc.Types.Pattern
import           Calc.Types.Type
import           Calc.TypeUtils
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Foldable          (traverse_)
import           Data.Functor           (($>))
import qualified Data.Map               as M
import qualified Data.Set               as S

data Drop = DropIdentifier Identifier
  deriving stock (Eq, Ord, Show)

getLinearityAnnotation :: Linearity ann -> ann
getLinearityAnnotation (Whole ann) = ann

validateModule :: (Show ann) => Module (Type ann) -> Either (LinearityError ann) ()
validateModule (Module {mdFunctions}) =
  traverse_ validateFunction mdFunctions

validateFunction :: (Show ann) => Function (Type ann) -> Either (LinearityError ann) (Expr [Drop])
validateFunction fn =
  let (expr, linearState) = getFunctionUses fn
   in validate linearState $> expr

validate :: LinearState ann -> Either (LinearityError ann) ()
validate (LinearState {lsVars, lsUses}) =
  let validateFunctionItem (ident, (linearity, ann)) =
        let completeUses = filterCompleteUses lsUses ident
         in case linearity of
              LTPrimitive ->
                if null completeUses
                  then Left (NotUsed ann ident)
                  else Right ()
              LTBoxed ->
                case length completeUses of
                  0 ->
                    Left (NotUsed ann ident)
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

getFunctionUses :: (Show ann) => Function (Type ann) -> (Expr [Drop], LinearState ann)
getFunctionUses (Function {fnBody, fnArgs}) =
  fst $ runIdentity $ runWriterT $ runStateT action initialState
  where
    action = decorate fnBody

    initialState =
      LinearState
        { lsVars = initialVars,
          lsUses = mempty
        }

    initialVars =
      foldMap
        ( \(FunctionArg {faAnn, faName = ArgumentName arg, faType}) ->
            M.singleton (Identifier arg) $ case faType of
              TPrim {} -> (LTPrimitive, getOuterTypeAnnotation faAnn)
              _        -> (LTBoxed, getOuterTypeAnnotation faAnn)
        )
        fnArgs

recordUse ::
  ( MonadState (LinearState ann) m,
    MonadWriter (S.Set Identifier) m
  ) =>
  Identifier ->
  ann ->
  m ()
recordUse ident ann = do
  modify (\ls -> ls {lsUses = (ident, Whole ann) : lsUses ls})
  tell (S.singleton ident)

addLetBinding ::
  (MonadState (LinearState ann) m) =>
  Pattern (Type ann) ->
  m (Pattern [Drop])
addLetBinding (PVar ty ident) = do
  let initialLinearity = case ty of
        TPrim {} -> LTPrimitive
        _        -> LTBoxed
  modify
    ( \ls ->
        ls
          { lsVars =
              M.insert
                ident
                (initialLinearity, getOuterTypeAnnotation ty)
                (lsVars ls)
          }
    )
  pure $ PVar mempty ident
addLetBinding (PWildcard _) = pure (PWildcard mempty)
addLetBinding (PBox _ pat) =
  PBox mempty <$> addLetBinding pat
addLetBinding (PTuple _ p ps) = do
  PTuple mempty
    <$> addLetBinding p
    <*> traverse addLetBinding ps

decorate ::
  (Show ann) =>
  (MonadState (LinearState ann) m, MonadWriter (S.Set Identifier) m) =>
  Expr (Type ann) ->
  m (Expr [Drop])
decorate (EVar ann ident) = do
  recordUse ident (getOuterTypeAnnotation ann)
  pure (EVar mempty ident)
decorate (ELet _ pat expr rest) = do
  ELet mempty
    <$> addLetBinding pat
    <*> decorate expr
    <*> decorate rest
decorate (EPrim _ prim) =
  pure $ EPrim mempty prim
decorate (EInfix _ op a b) =
  EInfix mempty op <$> decorate a <*> decorate b
decorate (EIf _ predExpr thenExpr elseExpr) = do
  (decoratedThen, thenIdents) <- runWriterT (decorate thenExpr)
  (decoratedElse, elseIdents) <- runWriterT (decorate elseExpr)

  -- work out idents used in the other branch but not this one
  let uniqueToThen = DropIdentifier <$> S.toList (S.difference thenIdents elseIdents)
      uniqueToElse = DropIdentifier <$> S.toList (S.difference elseIdents thenIdents)

  EIf mempty
    <$> decorate predExpr
    <*> pure (mapOuterExprAnnotation (const uniqueToElse) decoratedThen)
    <*> pure (mapOuterExprAnnotation (const uniqueToThen) decoratedElse)
decorate (EApply _ fnName args) =
  EApply mempty fnName <$> traverse decorate args
decorate (ETuple _ a as) =
  ETuple mempty <$> decorate a <*> traverse decorate as
decorate (EBox _ a) =
  EBox mempty <$> decorate a
decorate (EAnn _ ty a) =
  EAnn mempty (ty $> mempty) <$> decorate a
decorate (ELoad _ a) =
  ELoad mempty <$> decorate a
decorate (EStore _ a b) =
  EStore mempty <$> decorate a <*> decorate b
decorate (ESet _ identifier b) =
  ESet mempty identifier <$> decorate b
decorate (EBlock _ items) =
  EBlock mempty <$> decorate items
