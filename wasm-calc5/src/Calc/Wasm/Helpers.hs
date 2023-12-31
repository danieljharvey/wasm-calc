module Calc.Wasm.Helpers (memorySize) where

import Calc.Wasm.Types
import GHC.Natural

-- our memory is bits of i32s
memorySize :: WasmType -> Natural
memorySize I32 = 8
memorySize I64 = 16
memorySize F64 = 16
memorySize Pointer = memorySize I32
