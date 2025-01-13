{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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

import Calc.Linearity.Types
import Calc.TypeUtils
import Calc.Types.Identifier
import Calc.Types.Type
import Control.Monad (unless)
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import GHC.Natural

getFresh :: (MonadState (LinearState ann) m) => m Natural
getFresh = do
  modify (\ls -> ls {lsFresh = lsFresh ls + 1})
  gets lsFresh

-- | push a load of uses directly onto the head of the uses stack
pushUses ::
  (MonadState (LinearState ann) m) =>
  M.Map Identifier (NE.NonEmpty (Linearity ann)) ->
  m ()
pushUses uses = do
  let pushForIdent ident =
        traverse_ (recordUsesInState ident)
   in traverse_ (uncurry pushForIdent) (M.toList uses)

mapHead :: (a -> a) -> NE.NonEmpty a -> NE.NonEmpty a
mapHead f (neHead NE.:| neTail) =
  f neHead NE.:| neTail

recordUsesInState ::
  (MonadState (LinearState ann) m) =>
  Identifier ->
  Linearity ann ->
  m ()
recordUsesInState ident newItem = do
  modify
    ( \ls ->
        let f =
              M.alter
                ( \existing ->
                    Just $ case existing of
                      Just neExisting -> newItem NE.:| NE.toList neExisting
                      Nothing -> NE.singleton newItem
                )
                ident
         in ls {lsUses = mapHead f (lsUses ls)}
    )

recordUse ::
  ( MonadState (LinearState ann) m,
    MonadWriter (M.Map Identifier (Type ann)) m
  ) =>
  Identifier ->
  Type ann ->
  m ()
recordUse ident ty = do
  recordUsesInState ident (Whole $ getOuterTypeAnnotation ty)
  ignoreVars <- gets lsIgnoreVars
  unless (S.member ident ignoreVars || isPrimitive ty) $
    tell (M.singleton ident ty) -- we only want to track use of non-primitive types

recordReference ::
  ( MonadState (LinearState ann) m,
    MonadWriter (M.Map Identifier (Type ann)) m
  ) =>
  Identifier ->
  Type ann ->
  m ()
recordReference ident ty = do
  recordUsesInState ident (Borrow $ getOuterTypeAnnotation ty)
  ignoreVars <- gets lsIgnoreVars
  unless (S.member ident ignoreVars || isPrimitive ty) $
    tell (M.singleton ident ty) -- we only want to track use of non-primitive types

-- run an action, giving it a new uses scope
-- then chop off the new values and return them
-- this allows us to dedupe and re-add them to the current stack as desired
scoped ::
  (MonadState (LinearState ann) m) =>
  m a ->
  m (a, M.Map Identifier (NE.NonEmpty (Linearity ann)))
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
