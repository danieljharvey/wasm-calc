{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.Patterns (patternToPaths, typeFromPath, fromPath) where

import Calc.ExprUtils
import Calc.Types
import Calc.Wasm.Helpers
import Calc.Wasm.Types
import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import GHC.Natural

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
                patternToPaths pat (PathSelect (getOuterPatternAnnotation pat) (offsetList !! index) . addPath)
            )
              <$> zip [1 ..] (NE.toList ps)
          )

data Path ann
  = -- | we're going in deeper
    PathSelect (Type ann) Natural (Path ann)
  | -- | return this item
    PathFetch (Type ann)

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
