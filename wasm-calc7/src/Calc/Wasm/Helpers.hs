module Calc.Wasm.Helpers (boxed, memorySizeForType, getOffsetList, scalarFromType, memorySize) where

import Calc.Types.Type
import Calc.Wasm.Types
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import GHC.Natural

-- our memory is bits of i32s
memorySize :: WasmType -> Natural
memorySize I32 = 8
memorySize I64 = 16
memorySize F64 = 16
memorySize Pointer = memorySize I32
memorySize Void = 0

scalarFromType :: Type ann -> Either FromWasmError WasmType
scalarFromType (TPrim _ TInt) = pure I64
scalarFromType (TPrim _ TVoid) = pure Void
scalarFromType (TPrim _ TBool) = pure I32
scalarFromType (TPrim _ TFloat) = pure F64
scalarFromType (TFunction {}) = Left FunctionTypeNotScalar
scalarFromType (TContainer {}) = pure Pointer
scalarFromType (TVar _ _) =
  pure Pointer -- all polymorphic variables are Pointer
scalarFromType (TUnificationVar {}) =
  pure Pointer

getOffsetList :: Type ann -> [Natural]
getOffsetList (TContainer _ items) =
  scanl (\offset item -> offset + offsetForType item) 0 (NE.toList items)
getOffsetList _ = []

-- | size of the primitive in memory (ie, struct is size of its pointer)
offsetForType :: Type ann -> Natural
offsetForType (TPrim _ TInt) =
  memorySize I64
offsetForType (TPrim _ TFloat) =
  memorySize F64
offsetForType (TPrim _ TBool) =
  memorySize I32
offsetForType (TPrim _ TVoid) =
  error "offsetForType TVoid"
offsetForType (TContainer _ _) =
  memorySize Pointer
offsetForType (TFunction {}) =
  memorySize Pointer
offsetForType (TVar _ _) =
  memorySize Pointer
offsetForType (TUnificationVar _ _) =
  error "offsetForType TUnificationVar"

-- | wrap a `WasmExpr` in a single item struct
boxed :: Natural -> WasmType -> WasmExpr -> WasmExpr
boxed index ty wExpr =
  let allocate = WAllocate (memorySize ty)
   in WSet index allocate [(0, ty, wExpr)]

-- | the actual size of the item in memory
memorySizeForType :: Type ann -> Natural
memorySizeForType (TPrim _ TInt) =
  memorySize I64
memorySizeForType (TPrim _ TFloat) =
  memorySize F64
memorySizeForType (TPrim _ TBool) =
  memorySize I32
memorySizeForType (TPrim _ TVoid) =
  error "memorySizeForType TVoid"
memorySizeForType (TContainer _ as) =
  getSum (foldMap (Sum . memorySizeForType) as)
memorySizeForType (TFunction {}) =
  memorySize Pointer
memorySizeForType (TVar _ _) =
  memorySize Pointer
memorySizeForType (TUnificationVar _ _) =
  error "memorySizeForType TUnificationVar"
