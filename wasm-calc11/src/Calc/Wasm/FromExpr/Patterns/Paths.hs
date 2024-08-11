{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc.Wasm.FromExpr.Patterns.Paths
  ( patternToPaths,
    patternToDropPaths,
    typeFromPath,
    fromPath,
    Path (..),
  )
where

import Calc.ExprUtils
import Calc.Linearity (Drops (..))
import Calc.Types
import Calc.Wasm.FromExpr.Helpers
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import GHC.Natural

-- | for a variable, describe how to get it
data Path ann
  = -- | we're going in deeper
    PathSelect (Type ann) Natural (Path ann)
  | -- | return this item
    PathFetch (Type ann)
  deriving stock (Eq, Ord, Show, Functor)

-- | return a path to every item in Expr marked with `DropMe`.
patternToDropPaths ::
  ( Eq ann,
    MonadState FromExprState m,
    MonadError FromWasmError m
  ) =>
  Pattern (Type ann, Maybe (Drops ann)) ->
  (Path ann -> Path ann) ->
  m [Path ann]
patternToDropPaths (PWildcard (ty, drops)) addPath =
  pure [addPath (PathFetch ty) | drops == Just DropMe]
patternToDropPaths (PVar (ty, drops) _) addPath =
  pure [addPath (PathFetch ty) | drops == Just DropMe]
patternToDropPaths (PBox (ty, drops) a) addPath = do
  let dropContainer =
        ([addPath (PathFetch ty) | drops == Just DropMe])
  paths <- patternToDropPaths a (PathSelect (fst $ getOuterPatternAnnotation a) 0 . addPath)
  pure (paths <> dropContainer)
patternToDropPaths (PLiteral {}) _ =
  pure mempty
patternToDropPaths (PTuple (ty, drops) a as) addPath = do
  let offsetList = getOffsetList ty
  let dropContainer =
        ([addPath (PathFetch ty) | drops == Just DropMe])
  pathsHead <-
    patternToDropPaths a (PathSelect (fst $ getOuterPatternAnnotation a) (head offsetList) . addPath)
  pathsTail <-
    traverse
      ( \(index, innerPat) ->
          let innerTy = fst (getOuterPatternAnnotation innerPat)
           in patternToDropPaths innerPat (PathSelect innerTy (offsetList !! index) . addPath)
      )
      ( zip [1 ..] (NE.toList as)
      )
  pure (pathsHead <> mconcat pathsTail <> dropContainer)
patternToDropPaths (PConstructor (ty, drops) constructor ps) addPath = do
  offsetList <- getOffsetListForConstructor ty constructor
  let dropContainer =
        ([addPath (PathFetch ty) | drops == Just DropMe])
  paths <-
    traverse
      ( \(index, innerPat) ->
          let innerTy = fst (getOuterPatternAnnotation innerPat)
           in patternToDropPaths innerPat (PathSelect innerTy (offsetList !! index) . addPath)
      )
      (zip [1 ..] ps)

  pure (mconcat paths <> dropContainer)

patternToPaths ::
  (MonadError FromWasmError m, MonadState FromExprState m) =>
  Pattern (Type ann) ->
  (Path ann -> Path ann) ->
  m (M.Map Identifier (Path ann))
patternToPaths (PWildcard _) _ = pure mempty
patternToPaths (PLiteral {}) _ = pure mempty
patternToPaths (PVar ty ident) addPath =
  pure $ M.singleton ident (addPath (PathFetch ty))
patternToPaths (PBox _ pat) addPath =
  patternToPaths pat (PathSelect (getOuterPatternAnnotation pat) 0 . addPath)
patternToPaths (PTuple ty p ps) addPath = do
  let offsetList = getOffsetList ty

  pathsHead <- patternToPaths p (PathSelect (getOuterPatternAnnotation p) (head offsetList) . addPath)

  pathsTail <-
    traverse
      ( \(index, pat) ->
          let innerTy = getOuterPatternAnnotation pat
           in patternToPaths pat (PathSelect innerTy (offsetList !! index) . addPath)
      )
      (zip [1 ..] (NE.toList ps))

  pure (pathsHead <> mconcat pathsTail)
patternToPaths (PConstructor ty constructor ps) addPath = do
  offsetList <- getOffsetListForConstructor ty constructor
  paths <-
    traverse
      ( \(index, pat) ->
          let innerTy = getOuterPatternAnnotation pat
           in patternToPaths pat (PathSelect innerTy (offsetList !! index) . addPath)
      )
      (zip [1 ..] ps)

  pure (mconcat paths)

-- | given a path, create AST for fetching it
fromPath :: (MonadError FromWasmError m) => Natural -> Path ann -> m WasmExpr
fromPath wholeExprIndex (PathFetch _ty) =
  pure (WVar wholeExprIndex)
fromPath wholeExprIndex (PathSelect ty index inner) = do
  wasmTy <- liftEither (scalarFromType ty)
  innerExpr <- fromPath wholeExprIndex inner
  pure (WTupleAccess wasmTy innerExpr index)

typeFromPath :: Path ann -> Type ann
typeFromPath (PathSelect _ _ inner) = typeFromPath inner
typeFromPath (PathFetch ty) = ty
