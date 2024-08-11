{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc.Wasm.FromExpr.Patterns.Predicates where

import Calc.ExprUtils
import Calc.TypeUtils
import Calc.Types.Op
import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import Calc.Wasm.FromExpr.Helpers
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import GHC.Natural

data Predicate ann = Equals [(Type ann, Natural)] (Type ann) Prim
  deriving stock (Eq, Ord, Show)

-- | Return a list of things that would need to be true for a pattern to match
predicatesFromPattern ::
  ( MonadState FromExprState m,
    MonadError FromWasmError m
  ) =>
  Pattern (Type ann) ->
  [(Type ann, Natural)] ->
  m [Predicate ann]
predicatesFromPattern (PWildcard {}) _ = pure mempty
predicatesFromPattern (PLiteral ty prim) path = pure [Equals path ty prim]
predicatesFromPattern (PVar {}) _ = pure mempty
predicatesFromPattern (PBox _ inner) path =
  predicatesFromPattern inner (path <> [(getOuterPatternAnnotation inner, 0)])
predicatesFromPattern (PTuple ty p ps) path = do
  let allPs = zip (p : NE.toList ps) [0 ..]
  let offsetList = getOffsetList ty
  mconcat
    <$> traverse
      ( \(pat, index) ->
          predicatesFromPattern
            pat
            (path <> [(getOuterPatternAnnotation pat, offsetList !! index)])
      )
      allPs
predicatesFromPattern (PConstructor ty _constructor _) path = do
  -- what
  _dt <- case ty of
    TConstructor _ dataTypeName _ -> lookupDataType dataTypeName
    _ -> error "should be type"
  -- wrong but yolo
  let constructorValue = 0
  pure $ [Equals path (TPrim (getOuterTypeAnnotation ty) TInt32) (PIntLit constructorValue)]

-- | turn a single `Predicate` into a `WasmExpr` for that predicate, that
-- should return a boolean
predicateToWasm ::
  ( MonadState FromExprState m,
    MonadError FromWasmError m
  ) =>
  WasmExpr ->
  Predicate ann ->
  m WasmExpr
predicateToWasm wasmValue (Equals path tyPrim primValue) = do
  wasmPrim <- fromPrim tyPrim primValue
  wasmType <- liftEither $ scalarFromType tyPrim
  wasmPath <-
    reverse
      <$> traverse
        ( \(ty, index) ->
            (,)
              <$> liftEither (scalarFromType ty)
              <*> pure index
        )
        (reverse path)
  pure $ predicateToWasmInner wasmPath (WInfix wasmType OpEquals (WPrim wasmPrim)) wasmValue

-- | inner function that works on just Wasm IR types
predicateToWasmInner ::
  [(WasmType, Natural)] ->
  (WasmExpr -> WasmExpr) ->
  WasmExpr ->
  WasmExpr
predicateToWasmInner [] f wasmEquals = f wasmEquals
predicateToWasmInner ((ty, index) : rest) f wasmEquals =
  predicateToWasmInner rest f (WTupleAccess ty wasmEquals index)
