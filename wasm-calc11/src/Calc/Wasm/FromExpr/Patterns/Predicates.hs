{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc.Wasm.FromExpr.Patterns.Predicates where

import Calc.TypeUtils
import qualified Data.Map.Strict as M
import Calc.Types.DataName
import Calc.ExprUtils
import Calc.Types.Op
import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import Calc.Wasm.FromExpr.Helpers
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import GHC.Natural

data Predicate ann = Equals [(Type ann, Natural)] (Type ann) Prim
  deriving stock (Eq, Ord, Show)

-- | Return a list of things that would need to be true for a pattern to match
predicatesFromPattern :: M.Map DataName [FromExprConstructor] -> Pattern (Type ann) -> [(Type ann, Natural)] -> [Predicate ann]
predicatesFromPattern _ (PWildcard {}) _ = mempty
predicatesFromPattern _ (PLiteral ty prim) path = [Equals path ty prim]
predicatesFromPattern _ (PVar {}) _ = mempty
predicatesFromPattern dataTypes (PBox _ inner) path =
  predicatesFromPattern dataTypes inner (path <> [(getOuterPatternAnnotation inner, 0)])
predicatesFromPattern dataTypes (PTuple ty p ps) path =
  let allPs = zip (p : NE.toList ps) [0 ..]
      offsetList = getOffsetList ty
   in foldMap
        ( \(pat, index) ->
            predicatesFromPattern dataTypes
              pat
              (path <> [(getOuterPatternAnnotation pat, offsetList !! index)])
        )
        allPs
predicatesFromPattern _dataTypes (PConstructor ty _constructor _) path =
  -- what
  let _typeName = case ty of
                TConstructor _ tn _ -> tn
                _ -> error "should be type"
      -- wrong but yolo
      constructorValue = 1
   in [Equals path (TPrim (getOuterTypeAnnotation ty) TInt32) (PIntLit constructorValue)]

-- | turn a single `Predicate` into a `WasmExpr` for that predicate, that
-- should return a boolean
predicateToWasm :: (MonadError FromWasmError m) => WasmExpr -> Predicate ann -> m WasmExpr
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
