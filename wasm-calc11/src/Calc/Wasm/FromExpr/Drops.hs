{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.FromExpr.Drops
  ( DropPath (..),
    dropFunctionForType,
    addDropsFromPath,
    typeToDropPaths,
    createDropFunction,
    addDropsToWasmExpr,
  )
where

import Calc.Linearity (Drops (..))
import Calc.TypeUtils (monoidType)
import Calc.Types
import Calc.Wasm.FromExpr.Helpers
  ( addGeneratedFunction,
    genericArgName,
    getOffsetList,
    lookupIdent,
    scalarFromType,
  )
import Calc.Wasm.FromExpr.Patterns (Path (..))
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldl')
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Natural

-- | for a variable, describe how to get it
data DropPath ann
  = -- | we're going in deeper
    DropPathSelect (Type ann) Natural (DropPath ann)
  | -- | drop this item
    DropPathFetch (Maybe TypeVar)
  deriving stock (Eq, Ord, Show)

-- | given a type, create a new drop function for it or use one passed into the
-- current function
dropInstructionForType ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  WasmExpr ->
  Type ann ->
  m WasmExpr
dropInstructionForType itemIdentifier ty =
  case ty of
    TVar _ typeVar -> do
      -- generics must have been passed in as function args
      nat <- lookupIdent (genericArgName typeVar)
      pure (WApplyIndirect (WVar nat) [itemIdentifier])
    _ -> do
      -- generate a new fancy drop function
      dropFunc <- createDropFunction 1 ty
      dropVar <- addGeneratedFunction dropFunc
      pure (WApply dropVar [itemIdentifier])

-- | given a path, create AST for fetching it plus return type
dropsFromPath :: (MonadError FromWasmError m) => Natural -> Path ann -> m (Type ann, WasmExpr)
dropsFromPath wholeExprIndex (PathFetch ty) =
  pure (ty, WVar wholeExprIndex)
dropsFromPath wholeExprIndex (PathSelect ty index inner) = do
  wasmTy <- liftEither (scalarFromType ty)
  (innerTy, innerExpr) <- dropsFromPath wholeExprIndex inner
  pure (innerTy, WTupleAccess wasmTy innerExpr index)

addDropsFromPath ::
  ( MonadState FromExprState m,
    MonadError FromWasmError m
  ) =>
  Natural ->
  Path ann ->
  m WasmExpr
addDropsFromPath wholeExprIndex path = do
  (ty, wasmExpr) <- dropsFromPath wholeExprIndex path
  case ty of
    TVar _ typeVar -> do
      -- generics must have been passed in as function args
      nat <- lookupIdent (genericArgName typeVar)
      pure (WApplyIndirect (WVar nat) [wasmExpr])
    _ -> do
      pure $ WDrop wasmExpr

addDropsToWasmExpr ::
  ( MonadState FromExprState m,
    MonadError FromWasmError m
  ) =>
  Maybe (Drops ann) ->
  WasmExpr ->
  m WasmExpr
addDropsToWasmExpr drops wasmExpr =
  -- drop identifiers we will no longer need
  case drops of
    Just (DropIdentifiers idents) -> do
      nats <- traverse (\(ident, ty) -> (,) <$> lookupIdent ident <*> pure ty) idents
      foldM
        ( \restExpr (index, ty) -> do
            dropWasm <- dropInstructionForType (WVar index) ty
            pure $ WSequence Void dropWasm restExpr
        )
        wasmExpr
        nats
    Just DropMe -> pure wasmExpr -- TODO: what does this mean? anything?
    Nothing -> pure wasmExpr

typeToDropPaths ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  Type ann ->
  (DropPath ann -> DropPath ann) ->
  m [DropPath ann]
typeToDropPaths ty@(TContainer _ tyItems) addPath = do
  let offsetList = getOffsetList ty
  innerPaths <-
    traverse
      ( \(index, innerTy) ->
          typeToDropPaths
            innerTy
            ( DropPathSelect innerTy (offsetList !! index)
                . addPath
            )
      )
      (zip [0 ..] (NE.toList tyItems))

  pure
    ( mconcat innerPaths
        <> [addPath (DropPathFetch Nothing)]
    )
typeToDropPaths (TVar _ tyVar) addPath =
  pure [addPath (DropPathFetch (Just tyVar))]
typeToDropPaths _ _ = pure mempty

typeVars :: Type ann -> S.Set TypeVar
typeVars (TVar _ tv) = S.singleton tv
typeVars other = monoidType typeVars other

dropFunctionName :: Natural -> FunctionName
dropFunctionName i = FunctionName $ "drop_" <> T.pack (show i)

dropFunctionForType ::
  ( MonadState FromExprState m,
    MonadError FromWasmError m
  ) =>
  Type ann ->
  m WasmExpr
dropFunctionForType ty =
  case ty of
    TVar _ typeVar -> do
      -- generics must have been passed in as function args
      WVar <$> lookupIdent (genericArgName typeVar)
    _ -> do
      dropFunc <- createDropFunction 1 ty
      WFunctionPointer <$> addGeneratedFunction dropFunc

createDropFunction ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m
  ) =>
  Natural ->
  Type ann ->
  m WasmFunction
createDropFunction natIndex ty = do
  dropPaths <- typeToDropPaths ty id
  let typeVarList = S.toList (typeVars ty)
      allTypeVars = M.fromList $ zip typeVarList [0 ..]
  wasmTy <- liftEither (scalarFromType ty)
  let arg = 0
  wasmExprs <- traverse (dropExprFromPath allTypeVars arg) dropPaths

  let wasmArgs = wasmTy : (typeVarList $> Pointer)

  let expr = case wasmExprs of
        [] -> WReturnVoid
        _ -> flattenDropExprs wasmExprs

  pure $
    WasmFunction
      { wfName = dropFunctionName natIndex,
        wfExpr = expr,
        wfPublic = False,
        wfArgs = wasmArgs,
        wfReturnType = Void,
        wfLocals = mempty,
        wfAbilities = mempty
      }

-- | do all the drops one after the other
-- fails if list is empty
flattenDropExprs :: [(Maybe Natural, WasmExpr)] -> WasmExpr
flattenDropExprs exprs = case NE.uncons (NE.fromList exprs) of
  ((Nothing, a), Nothing) -> WDrop a
  ((Just i, a), Nothing) -> WApply (WasmGeneratedRef i) [a]
  (starting, Just rest) ->
    let withDrop (dropType, a) = case dropType of
          Just i -> WApply (WasmGeneratedRef i) [a]
          Nothing -> WDrop a
     in foldl'
          ( \exprA exprB ->
              WSequence Void exprA (withDrop exprB)
          )
          (withDrop starting)
          rest

-- | given a path, create AST for fetching it
dropExprFromPath ::
  (MonadError FromWasmError m) =>
  M.Map TypeVar Natural ->
  Natural ->
  DropPath ann ->
  m (Maybe Natural, WasmExpr)
dropExprFromPath _ wholeExprIndex (DropPathFetch Nothing) =
  pure (Nothing, WVar wholeExprIndex)
dropExprFromPath typeVarMap wholeExprIndex (DropPathFetch (Just tyVar)) =
  case M.lookup tyVar typeVarMap of
    Just i ->
      pure (Just i, WVar wholeExprIndex)
    Nothing -> error "Failed finding generic"
dropExprFromPath typeVarMap wholeExprIndex (DropPathSelect ty index inner) = do
  wasmTy <- liftEither (scalarFromType ty)
  (nat, innerExpr) <- dropExprFromPath typeVarMap wholeExprIndex inner
  pure (nat, WTupleAccess wasmTy innerExpr index)
