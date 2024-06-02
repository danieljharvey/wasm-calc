{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.FromExpr.Drops
  ( DropPath (..),
    typeToDropPaths,
    createDropFunction,
  )
where

import Calc.TypeUtils (monoidType)
import Calc.Types
import Calc.Wasm.FromExpr.Helpers (scalarFromType)
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Helpers
import Calc.Wasm.ToWasm.Types
import Control.Monad.Except
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

typeToDropPaths ::
  Type ann ->
  (DropPath ann -> DropPath ann) ->
  [DropPath ann]
typeToDropPaths ty@(TContainer _ tyItems) addPath =
  let offsetList = getOffsetList ty
   in mconcat
        ( ( \(index, innerTy) ->
              typeToDropPaths
                innerTy
                ( DropPathSelect innerTy (offsetList !! index)
                    . addPath
                )
          )
            <$> zip [0 ..] (NE.toList tyItems)
        )
        <> [addPath (DropPathFetch Nothing)]
typeToDropPaths (TVar _ tyVar) addPath =
  [addPath (DropPathFetch (Just tyVar))]
typeToDropPaths _ _ = mempty

typeVars :: Type ann -> S.Set TypeVar
typeVars (TVar _ tv) = S.singleton tv
typeVars other = monoidType typeVars other

dropFunctionName :: Natural -> FunctionName
dropFunctionName i = FunctionName $ "drop_" <> T.pack (show i)

createDropFunction :: (MonadError FromWasmError m) => Natural -> Type ann -> m WasmFunction
createDropFunction natIndex ty = do
  let dropPaths = typeToDropPaths ty id
      typeVarList = S.toList (typeVars ty)
      allTypeVars = M.fromList $ zip typeVarList [0 ..]
  wasmTy <- liftEither (scalarFromType ty)
  let arg = 0
  wasmExprs <- traverse (dropExprFromPath allTypeVars arg) dropPaths

  let wasmArgs = wasmTy : (typeVarList $> Pointer)

  let expr = case wasmExprs of
        [] -> WDrop (WVar 0) -- no
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
