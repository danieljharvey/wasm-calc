module Calc.Wasm.Helpers (memorySize) where

import Calc.Wasm.Types
import GHC.Natural

-- our memory is bits of i32s
memorySize :: WasmType -> Natural
memorySize I32 = 1
memorySize Pointer = 1

