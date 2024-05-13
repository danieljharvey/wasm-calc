{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Calc.Wasm.FromExpr.Drops
  ( DropPath (..),
    typeToDropPaths,
    createDropFunction,
  )
where

import           Calc.Types
import           Calc.TypeUtils             (monoidType)
import           Calc.Wasm.FromExpr.Helpers (scalarFromType)
import           Calc.Wasm.FromExpr.Types
import           Calc.Wasm.ToWasm.Helpers
import           Calc.Wasm.ToWasm.Types
import           Control.Monad.Except
import           Data.Foldable              (foldl')
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Debug.Trace
import           GHC.Natural

-- | for a variable, describe how to get it
data DropPath ann
  = -- | we're going in deeper
    DropPathSelect (Type ann) Natural (DropPath ann)
  | -- | drop this item
    DropPathFetch (Maybe TypeVar)
  deriving stock (Eq, Ord, Show)

typeToDropPaths :: Type ann -> (DropPath ann -> DropPath ann) -> [DropPath ann]
typeToDropPaths ty@(TContainer _ tyItems) addPath =
  let offsetList = getOffsetList ty
   in mconcat
        ( ( \(index, innerTy) ->
              typeToDropPaths innerTy (DropPathSelect innerTy (offsetList !! index) . addPath)
          )
            <$> zip [0 ..] (NE.toList tyItems)
        )
        <> [addPath (DropPathFetch Nothing)]
typeToDropPaths (TVar _ tyVar) addPath =
  [addPath (DropPathFetch (Just tyVar))]
typeToDropPaths _ _ = mempty

typeVars :: Type ann -> S.Set TypeVar
typeVars (TVar _ tv) = S.singleton tv
typeVars other       = monoidType typeVars other

createDropFunction :: (MonadError FromWasmError m,Show ann) => Type ann -> m WasmFunction
createDropFunction ty = do
  let dropPaths = traceShowId $ typeToDropPaths ty id
      allTypeVars = M.fromList $ zip (S.toList (typeVars ty)) [0 ..]
  wasmTy <- liftEither (scalarFromType ty)
  let arg = 0
  wasmExprs <- traverse (dropExprFromPath allTypeVars arg) dropPaths

  pure $
    WasmFunction
      { wfName = FunctionName "drop_1",
        wfExpr = flattenDropExprs (traceShowId wasmExprs),
        wfPublic = False,
        wfArgs = [wasmTy], -- ie, the type of the thing
        wfReturnType = Void,
        wfLocals = [],
        wfAbilities = mempty
      }

-- | do all the drops one after the other
-- fails if list is empty
flattenDropExprs :: [WasmExpr] -> WasmExpr
flattenDropExprs exprs = case NE.uncons (NE.fromList exprs) of
  (a, Nothing)   -> WDrop a
  (a, Just rest) -> foldl' (\exprA exprB ->
        WSequence Void exprA (WDrop exprB)) (WDrop a) rest

-- | given a path, create AST for fetching it
dropExprFromPath ::
  (MonadError FromWasmError m) =>
  M.Map TypeVar Natural ->
  Natural ->
  DropPath ann ->
  m WasmExpr
dropExprFromPath _ wholeExprIndex (DropPathFetch Nothing) =
  pure (WVar wholeExprIndex)
dropExprFromPath typeVarMap wholeExprIndex (DropPathFetch (Just tyVar)) =
  case M.lookup tyVar typeVarMap of
    Just i ->
      pure (WApply (WasmGeneratedRef i) [WVar wholeExprIndex])
    Nothing -> error "sdfs"
dropExprFromPath typeVarMap wholeExprIndex (DropPathSelect ty index inner) = do
  wasmTy <- liftEither (scalarFromType ty)
  innerExpr <- dropExprFromPath typeVarMap wholeExprIndex inner
  pure (WTupleAccess wasmTy innerExpr index)
