module Calc.Wasm.ToWasm.Helpers
  ( globalOffset,
    getOffsetList,
    boxed,
    memorySize,
    memorySizeForType,
    offsetForType,
  )
where

import           Calc.Types
import           Calc.Wasm.ToWasm.Types
import qualified Data.List.NonEmpty     as NE
import           Data.Monoid
import           GHC.Natural

-- when should user-specified globals begin? after all the allocator ones!
globalOffset :: Natural
globalOffset = 1

-- 1 item is a byte, so i8, so i32 is 4 bytes
memorySize :: WasmType -> Natural
memorySize I8      = 1
memorySize I16     = 2
memorySize I32     = 4
memorySize I64     = 8
memorySize F32     = 4
memorySize F64     = 8
memorySize Pointer = memorySize I32
memorySize Void    = 0

-- | wrap a `WasmExpr` in a single item struct
boxed :: Natural -> Natural -> WasmType -> WasmExpr -> WasmExpr
boxed importsSize index ty wExpr =
  let allocate = WAllocate importsSize (memorySize ty)
   in WSet index allocate [(0, ty, wExpr)]

getOffsetList :: Type ann -> [Natural]
getOffsetList (TContainer _ items) =
  scanl (\offset item -> offset + offsetForType item) 0 (NE.toList items)
getOffsetList _ = []

-- | size of the primitive in memory (ie, struct is size of its pointer)
offsetForType :: Type ann -> Natural
offsetForType (TPrim _ TInt8) =
  memorySize I8
offsetForType (TPrim _ TInt16) =
  memorySize I16
offsetForType (TPrim _ TInt32) =
  memorySize I32
offsetForType (TPrim _ TInt64) =
  memorySize I64
offsetForType (TPrim _ TFloat32) =
  memorySize F32
offsetForType (TPrim _ TFloat64) =
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

-- | the actual size of the item in memory
memorySizeForType :: Type ann -> Natural
memorySizeForType (TPrim _ TInt8) = memorySize I8
memorySizeForType (TPrim _ TInt16) =
  memorySize I16
memorySizeForType (TPrim _ TInt32) =
  memorySize I32
memorySizeForType (TPrim _ TInt64) =
  memorySize I64
memorySizeForType (TPrim _ TFloat32) =
  memorySize F32
memorySizeForType (TPrim _ TFloat64) =
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
