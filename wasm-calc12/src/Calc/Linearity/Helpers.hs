{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Calc.Linearity.Helpers
  ( getFresh,
    pushUses,
    mapHead,
    recordUse,
    recordReference,
    scoped,
    isPrimitive,
    dropForType,
    getVarsInScope,
    combineWithBiggestItems,
  )
where

import Calc.Linearity.Error
import Calc.Linearity.Types
import Calc.TypeUtils
import Calc.Types.Identifier
import Calc.Types.Type
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import GHC.Natural

-- | push a load of uses directly onto the head of the uses stack
pushUses ::
  ( MonadError (LinearityError ann) m,
    MonadState (LinearState ann) m
  ) =>
  M.Map Identifier (LinState ann, Type ann) ->
  m ()
pushUses uses =
  traverse_ (\(i, (ls, ty)) -> recordUsesInState i ty ls) (M.toList uses)

recordUsesInState ::
  ( MonadState (LinearState ann) m,
    MonadError (LinearityError ann) m
  ) =>
  Identifier ->
  Type ann ->
  LinState ann ->
  m ()
recordUsesInState ident ty linState = do
  existing <- gets (M.lookup ident . NE.head . lsUses)
  case (linState, existing) of
    (Used ann, Just (Used usedAnn, _)) ->
      throwError $ UsedMultipleTimes ann usedAnn ident
    (Borrow ann, Just (Used usedAnn, _)) ->
      throwError $ BorrowAfterUse usedAnn ann ident
    _ -> pure ()
  modify
    ( \ls ->
        let f = M.insert ident (linState, ty)
         in ls {lsUses = mapHead f (lsUses ls)}
    )

recordUse ::
  ( MonadState (LinearState ann) m,
    MonadError (LinearityError ann) m
  ) =>
  Identifier ->
  Type ann ->
  m ()
recordUse ident ty = do
  ignoreVars <- gets lsIgnoreVars
  if S.member ident ignoreVars || isPrimitive ty
    then recordUsesInState ident ty (Fresh (getOuterTypeAnnotation ty))
    else do
      recordUsesInState ident ty (Used (getOuterTypeAnnotation ty))

recordReference ::
  ( MonadState (LinearState ann) m,
    MonadError (LinearityError ann) m
  ) =>
  Identifier ->
  Type ann ->
  m ()
recordReference ident ty = do
  ignoreVars <- gets lsIgnoreVars
  if S.member ident ignoreVars || isPrimitive ty
    then recordUsesInState ident ty (Fresh (getOuterTypeAnnotation ty))
    else do
      recordUsesInState ident ty (Borrow (getOuterTypeAnnotation ty))

-- run an action, giving it a new uses scope
-- then chop off the new values and return them
-- this allows us to dedupe and re-add them to the current stack as desired
scoped ::
  (MonadState (LinearState ann) m) =>
  m a ->
  m (a, M.Map Identifier (LinState ann, Type ann))
scoped action = do
  -- add a new empty stack
  modify (\ls -> ls {lsUses = mempty NE.:| NE.toList (lsUses ls)})
  -- run the action, collecting uses in NE.head of uses stack
  result <- action
  -- grab the top level items
  items <- gets (NE.head . lsUses)
  -- bin them off stack
  modify (\ls -> ls {lsUses = NE.fromList (NE.tail (lsUses ls))})
  -- return both things
  pure (result, items)

getFresh :: (MonadState (LinearState ann) m) => m Natural
getFresh = do
  modify (\ls -> ls {lsFresh = lsFresh ls + 1})
  gets lsFresh

mapHead :: (a -> a) -> NE.NonEmpty a -> NE.NonEmpty a
mapHead f (neHead NE.:| neTail) =
  f neHead NE.:| neTail

isPrimitive :: Type ann -> Bool
isPrimitive (TPrim {}) = True
isPrimitive _ = False

dropForType :: Type ann -> Maybe (Drops an)
dropForType ty = if isPrimitive ty then Nothing else Just DropMe

getVarsInScope :: (MonadState (LinearState ann) m) => m (S.Set Identifier)
getVarsInScope = gets (S.fromList . mapMaybe userDefined . M.keys . lsVars)
  where
    userDefined = \case
      UserDefined i -> Just i
      _ -> Nothing

combineWithBiggestItems ::
  (Ord k, Foldable t) =>
  M.Map k (t a) ->
  M.Map k (t a) ->
  M.Map k (t a)
combineWithBiggestItems =
  M.unionWith (\l r -> if length r > length l then r else l)
