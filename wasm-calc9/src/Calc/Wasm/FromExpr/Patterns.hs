{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc.Wasm.FromExpr.Patterns
  ( DropPath (..),
    typeToPaths,
    exprToPaths,
    dropFromPath,
    patternToPaths,
    typeFromPath,
    fromPath,
  )
where

import Calc.ExprUtils
import Calc.Linearity (Drops (..))
import Calc.Types
import Calc.Wasm.FromExpr.Helpers
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Helpers
import Calc.Wasm.ToWasm.Types
import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import GHC.Natural

-- | for a variable, describe how to get it
data DropPath
  = -- | we're going in deeper
    DropPathSelect Natural DropPath
  | -- | drop this item
    DropPathFetch (Maybe TypeVar)
  deriving stock (Eq, Ord, Show)

typeToPaths :: Type ann -> (DropPath -> DropPath) -> [DropPath]
typeToPaths ty@(TContainer _ tyItems) addPath =
  let offsetList = getOffsetList ty
   in mconcat
        ( ( \(index, innerTy) ->
              typeToPaths innerTy (DropPathSelect (offsetList !! index) . addPath)
          )
            <$> zip [0 ..] (NE.toList tyItems)
        )
        <> [addPath (DropPathFetch Nothing)]
typeToPaths (TVar _ tyVar) addPath =
  [addPath (DropPathFetch (Just tyVar))]
typeToPaths _ _ = mempty

-- | return a path to every item in Expr marked with `DropMe`.
exprToPaths ::
  Expr (Type ann, Maybe Drops) ->
  (Path ann -> Path ann) ->
  [Path ann]
exprToPaths (EBox (ty, drops) a) addPath =
  let dropContainer =
        ([addPath (PathFetch ty) | drops == Just DropMe])
   in dropContainer
        <> exprToPaths a (PathSelect (fst $ getOuterAnnotation a) 0 . addPath)
exprToPaths (ETuple (ty, drops) a as) addPath =
  let offsetList = getOffsetList ty
      dropContainer =
        ([addPath (PathFetch ty) | drops == Just DropMe])
   in dropContainer
        <> exprToPaths a (PathSelect (fst $ getOuterAnnotation a) (head offsetList) . addPath)
        <> mconcat
          ( ( \(index, innerExpr) ->
                let innerTy = fst (getOuterAnnotation innerExpr)
                 in exprToPaths innerExpr (PathSelect innerTy (offsetList !! index) . addPath)
            )
              <$> zip [1 ..] (NE.toList as)
          )
exprToPaths _ _ = mempty

patternToPaths ::
  Pattern (Type ann) ->
  (Path ann -> Path ann) ->
  M.Map Identifier (Path ann)
patternToPaths (PWildcard _) _ = mempty
patternToPaths (PVar ty ident) addPath =
  M.singleton ident (addPath (PathFetch ty))
patternToPaths (PBox _ pat) addPath =
  patternToPaths pat (PathSelect (getOuterPatternAnnotation pat) 0 . addPath)
patternToPaths (PTuple ty p ps) addPath =
  let offsetList = getOffsetList ty
   in patternToPaths p (PathSelect (getOuterPatternAnnotation p) (head offsetList) . addPath)
        <> mconcat
          ( ( \(index, pat) ->
                let innerTy = getOuterPatternAnnotation pat
                 in patternToPaths pat (PathSelect innerTy (offsetList !! index) . addPath)
            )
              <$> zip [1 ..] (NE.toList ps)
          )

-- | for a variable, describe how to get it
data Path ann
  = -- | we're going in deeper
    PathSelect (Type ann) Natural (Path ann)
  | -- | return this item
    PathFetch (Type ann)
  deriving stock (Eq, Ord, Show, Functor)

-- | given a path, create AST for fetching it
fromPath :: (MonadError FromWasmError m) => Natural -> Path ann -> m WasmExpr
fromPath wholeExprIndex (PathFetch _ty) =
  pure (WVar wholeExprIndex)
fromPath wholeExprIndex (PathSelect ty index inner) = do
  wasmTy <- liftEither (scalarFromType ty)
  innerExpr <- fromPath wholeExprIndex inner
  pure (WTupleAccess wasmTy innerExpr index)

-- | given a path, create AST for fetching it
dropFromPath :: (MonadError FromWasmError m) => Natural -> Path ann -> m WasmExpr
dropFromPath wholeExprIndex (PathFetch _ty) =
  pure (WDrop (WVar wholeExprIndex))
dropFromPath wholeExprIndex (PathSelect ty index inner) = do
  wasmTy <- liftEither (scalarFromType ty)
  innerExpr <- dropFromPath wholeExprIndex inner
  pure (WTupleAccess wasmTy innerExpr index)

typeFromPath :: Path ann -> Type ann
typeFromPath (PathSelect _ _ inner) = typeFromPath inner
typeFromPath (PathFetch ty) = ty
