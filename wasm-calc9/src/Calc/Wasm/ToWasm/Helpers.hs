{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.ToWasm.Helpers
  ( UsesAllocator (..),
    moduleUsesAllocator,
    getOffsetList,
    allocCountIndex,
    boxed,
    memorySize,
    memorySizeForType,
    offsetForType,
    testName,
    allocIndex,
    dropIndex,
    functionOffset,
    globalOffset,
    generatedFunctionOffset,
  )
where

import Calc.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad.Reader
import Data.Bool (bool)
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Natural

data UsesAllocator = UsesAllocator | DoesNotUseAllocator
  deriving stock (Eq, Ord, Show)

moduleUsesAllocator :: WasmModule -> UsesAllocator
moduleUsesAllocator =
  bool DoesNotUseAllocator UsesAllocator
    . getAny
    . foldMap (Any . S.member (AllocateMemory ()) . wfAbilities)
    . wmFunctions

allocIndex :: (MonadReader ToWasmEnv m) => m Natural
allocIndex = asks tweImportsOffset

dropIndex :: (MonadReader ToWasmEnv m) => m Natural
dropIndex = asks ((+ 1) . tweImportsOffset)

allocCountIndex :: (MonadReader ToWasmEnv m) => m Natural
allocCountIndex = asks ((+ 2) . tweImportsOffset)

functionOffset :: ToWasmEnv -> Natural
functionOffset
  twe = tweImportsOffset twe + tweFunctionsOffset twe

generatedFunctionOffset :: ToWasmEnv -> Natural
generatedFunctionOffset
  twe =
    tweImportsOffset twe
      + tweFunctionsOffset twe
      + tweGeneratedFunctionOffset twe

globalOffset :: (MonadReader ToWasmEnv m) => m Natural
globalOffset = asks tweGlobalOffset

-- what do we call tests in Wasm exports
testName :: WasmTest -> T.Text
testName (WasmTest {wtName}) = "_test_" <> wtName

-- 1 item is a byte, so i8, so i32 is 4 bytes
memorySize :: WasmType -> Natural
memorySize I8 = 1
memorySize I16 = 2
memorySize I32 = 4
memorySize I64 = 8
memorySize F32 = 4
memorySize F64 = 8
memorySize Pointer = memorySize I32
memorySize Void = 0

-- | wrap a `WasmExpr` in a single item struct
boxed :: Natural -> WasmType -> WasmExpr -> WasmExpr
boxed index ty wExpr =
  let allocate = WAllocate (memorySize ty)
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
