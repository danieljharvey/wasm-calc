{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}

module Calc.Wasm.FromExpr.Drops
  ( DropPath (..),
    typeToDropPaths,dropFunctionFromPath
  )
where

import           Calc.Types
import           Calc.Wasm.FromExpr.Helpers (scalarFromType)
import           Calc.Wasm.FromExpr.Types
import           Calc.Wasm.ToWasm.Helpers
import           Calc.Wasm.ToWasm.Types
import           Control.Monad.Except
import qualified Data.List.NonEmpty         as NE
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

createDropFunction :: Natural -> DropPath ann -> m WasmFunction
createDropFunction wholeExprIndex dropPath = do
  let allTypeVars = mempty -- _ dropPath
  wasmExpr <- dropExprFromPath wholeExprIndex dropPath

  pure $ WasmFunction  { wfName :: FunctionName,
    wfExpr :: WasmExpr,
    wfPublic :: Bool,
    wfArgs :: [WasmType],
    wfReturnType :: WasmType,
    wfLocals :: [WasmType],
    wfAbilities :: S.Set (Ability ())
  }

-- | given a path, create AST for fetching it
dropExprFromPath :: (MonadError FromWasmError m) => Natural -> DropPath ann -> m WasmExpr
dropExprFromPath wholeExprIndex (DropPathFetch Nothing) =
  pure (WDrop (WVar wholeExprIndex))
dropExprFromPath wholeExprIndex (DropPathFetch (Just tyVar)) =
  pure (WApply _ (WVar wholeExprIndex))
dropExprFromPath wholeExprIndex (DropPathSelect ty index inner) = do
  wasmTy <- liftEither (scalarFromType ty)
  innerExpr <- dropExprFromPath wholeExprIndex inner
  pure (WTupleAccess wasmTy innerExpr index)
