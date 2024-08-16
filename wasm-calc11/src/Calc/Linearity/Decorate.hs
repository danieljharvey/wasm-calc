{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Calc.Linearity.Decorate
  ( decorate,
  )
where

import Data.Foldable (traverse_)
import Debug.Trace
import Calc.ExprUtils
import Calc.Linearity.Types
import Calc.TypeUtils
import Calc.Types.Expr
import Calc.Types.Identifier
import Calc.Types.Pattern
import Calc.Types.Type
import Control.Monad (unless)
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (second)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Natural

getFresh :: (MonadState (LinearState ann) m) => m Natural
getFresh = do
  modify (\ls -> ls {lsFresh = lsFresh ls + 1})
  gets lsFresh

-- | push a load of uses directly onto the head of the uses stack
pushUses :: (MonadState (LinearState ann) m) =>
  M.Map Identifier (NE.NonEmpty (Linearity ann)) -> m ()
pushUses uses =
  let pushForIdent ident items
          = traverse_ (\(Whole ann) -> recordUsesInState ident ann) items
  in traverse_ (uncurry pushForIdent) (M.toList uses)

mapHead :: (a -> a) -> NE.NonEmpty a -> NE.NonEmpty a
mapHead f (neHead NE.:| neTail) =
  (f neHead) NE.:| neTail

recordUsesInState :: (MonadState (LinearState ann) m) =>
  Identifier -> ann -> m()
recordUsesInState ident ann =
  modify (\ls ->
      let f =
            M.alter (\existing ->
                      let newItem = Whole ann
                       in Just $ case existing of
                                   Just neExisting -> newItem NE.:| (NE.toList neExisting)
                                   Nothing -> NE.singleton newItem) ident
       in
    ls {lsUses = mapHead f (lsUses ls) })


recordUse ::
  ( MonadState (LinearState ann) m,
    MonadWriter (M.Map Identifier (Type ann)) m
  ) =>
  Identifier ->
  Type ann ->
  m ()
recordUse ident ty = do
  recordUsesInState ident (getOuterTypeAnnotation ty)
  unless (isPrimitive ty) $ tell (M.singleton ident ty) -- we only want to track use of non-primitive types

-- run an action, giving it a new uses scope
-- then chop off the new values and return them
-- this allows us to dedupe and re-add them to the current stack as desired
scoped :: (MonadState (LinearState ann) m) => m a -> m (a, (M.Map Identifier (NE.NonEmpty (Linearity ann))))
scoped action = do
  -- add a new empty stack
  modify (\ls -> ls { lsUses = mempty NE.:| (NE.toList $ lsUses ls) })
  -- run the action, collecting uses in NE.head of uses stack
  result <- action
  -- grab the top level items
  items <- gets (NE.head . lsUses)
  -- bin them off stack
  modify (\ls -> ls { lsUses = NE.fromList (NE.tail (lsUses ls)) })
  -- return both things
  pure (result, items)


isPrimitive :: Type ann -> Bool
isPrimitive (TPrim {}) = True
isPrimitive _ = False

addLetBinding ::
  (MonadState (LinearState ann) m) =>
  Pattern (Type ann) ->
  m (Pattern (Type ann, Maybe (Drops ann)))
addLetBinding pat = do
  (decoratedPat, idents) <- decoratePattern pat
  modify
    ( \ls ->
        ls
          { lsVars = idents <> lsVars ls
          }
    )
  pure decoratedPat

decoratePattern ::
  (MonadState (LinearState ann) m) =>
  Pattern (Type ann) ->
  m
    ( Pattern (Type ann, Maybe (Drops ann)),
      M.Map (UserDefined Identifier) (LinearityType, ann)
    )
decoratePattern (PVar ty ident) = do
  let idents =
        M.singleton
          (UserDefined ident)
          ( if isPrimitive ty then LTPrimitive else LTBoxed,
            getOuterTypeAnnotation ty
          )
  pure (PVar (ty, Nothing) ident, idents)
decoratePattern (PWildcard ty) = do
  case ty of
    TPrim _ TVoid -> pure (PWildcard (ty, Nothing), mempty)
    _ -> do
      i <- getFresh
      let ident = Identifier $ "_fresh_name" <> T.pack (show i)
          idents =
            M.singleton
              (Internal ident)
              ( if isPrimitive ty then LTPrimitive else LTBoxed,
                getOuterTypeAnnotation ty
              )
      pure (PVar (ty, dropForType ty) ident, idents)
decoratePattern (PLiteral ty prim) =
  pure (PLiteral (ty, Nothing) prim, mempty)
decoratePattern (PBox ty pat) = do
  (decoratedPat, innerIdents) <- decoratePattern pat
  pure (PBox (ty, dropForType ty) decoratedPat, innerIdents)
decoratePattern (PConstructor ty constructor pats) = do
  decoratedPatsAndIdents <- traverse decoratePattern pats

  let allIdents = foldMap snd decoratedPatsAndIdents

  pure (PConstructor (ty, dropForType ty) constructor (fst <$> decoratedPatsAndIdents), allIdents)
decoratePattern (PTuple ty p ps) = do
  (decoratedPat, innerIdents) <- decoratePattern p
  decoratedPatsAndIdents <- traverse decoratePattern ps

  let allIdents = innerIdents <> foldMap snd decoratedPatsAndIdents

  pure
    ( PTuple
        (ty, dropForType ty)
        decoratedPat
        (fst <$> decoratedPatsAndIdents),
      allIdents
    )

dropForType :: Type ann -> Maybe (Drops an)
dropForType ty = if isPrimitive ty then Nothing else Just DropMe

getVarsInScope :: (MonadState (LinearState ann) m) => m (S.Set Identifier)
getVarsInScope = gets (S.fromList . mapMaybe userDefined . M.keys . lsVars)
  where
    userDefined = \case
      UserDefined i -> Just i
      _ -> Nothing

decorate ::
  (Show ann) =>
  ( MonadState (LinearState ann) m,
    MonadWriter (M.Map Identifier (Type ann)) m
  ) =>
  Expr (Type ann) ->
  m (Expr (Type ann, Maybe (Drops ann)))
decorate (EVar ty ident) = do
  recordUse ident ty
  pure (EVar (ty, Nothing) ident)
decorate (EConstructor ty constructor args) = do
  EConstructor (ty, Nothing) constructor <$> traverse decorate args
decorate (ELet ty pat expr rest) = do
  -- get all idents mentioned in `expr`
  decoratedExpr <- decorate expr

  ELet (ty, Nothing)
    <$> addLetBinding pat
    <*> pure decoratedExpr
    <*> decorate rest -- keep hold of the stuff we learned
decorate (EPrim ty prim) =
  pure $ EPrim (ty, Nothing) prim
decorate (EMatch ty expr pats) = do
  decoratedExpr <- decorate expr

  -- we're only interested in adding drops
  -- for vars currently in scope outside the pattern arms
  existingVars <- getVarsInScope

  -- need to work out a way of scoping variables created in patterns
  -- as they only exist in `patExpr`
  let decoratePair (pat, patExpr) = do
        (decoratedPat, _idents) <- decoratePattern pat
        (decoratedPatExpr, patIdents) <- runWriterT (decorate patExpr)
        -- we only care about idents that exist in the current scope
        let usefulIdents =
              M.filterWithKey (\k _ -> S.member k existingVars) patIdents
        pure (usefulIdents, (decoratedPat, decoratedPatExpr))

  decoratedPatterns <- traverse decoratePair pats

  let allIdents = foldMap fst decoratedPatterns

  -- now we know all the idents, we can decorate each pattern with the ones
  -- it's missing
  let decorateWithIdents (idents, (pat, patExpr)) =
        let dropIdents = DropIdentifiers <$> NE.nonEmpty (M.toList (M.difference allIdents idents))
         in (pat, mapOuterExprAnnotation (second (const dropIdents)) patExpr)

  let decoratedPatternsWithIdents = decorateWithIdents <$> decoratedPatterns

  pure $ EMatch (ty, Nothing) decoratedExpr decoratedPatternsWithIdents
decorate (EInfix ty op a b) =
  EInfix (ty, Nothing) op <$> decorate a <*> decorate b
decorate (EIf ty predExpr thenExpr elseExpr) = do
  ((decoratedThen,thenUses), thenIdents) <- runWriterT (scoped (decorate thenExpr))
  ((decoratedElse, elseUses), elseIdents) <- runWriterT (scoped (decorate elseExpr))

  traceShowM ("thenUses" :: String, thenUses)
  traceShowM ("elseUses" :: String, elseUses)

  -- here we're gonna go through each constructor and keep the longest list of
  -- things
  let usesToKeep = thenUses <> elseUses
  _ <- error "here is the problem"

  traceShowM ("usesToKeep" :: String, usesToKeep)

  -- push the ones we want to keep hold of
  pushUses usesToKeep

  -- work out idents used in the other branch but not this one
  let uniqueToThen = DropIdentifiers <$> NE.nonEmpty (M.toList (M.difference thenIdents elseIdents))
      uniqueToElse = DropIdentifiers <$> NE.nonEmpty (M.toList (M.difference elseIdents thenIdents))

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
