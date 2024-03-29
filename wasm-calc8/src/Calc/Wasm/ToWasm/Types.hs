{-# LANGUAGE DerivingStrategies #-}

module Calc.Wasm.ToWasm.Types
  ( WasmType (..),
    WasmPrim (..),
    WasmModule (..),
    WasmGlobal (..),
    WasmMemory (..),
    WasmFunction (..),
    WasmExpr (..),
    WasmImport (..),
  )
where

import Calc.Types.Function
import Calc.Types.Op
import qualified Data.Text as T
import Data.Word
import GHC.Natural

data WasmType
  = I8
  | I16
  | I32
  | I64
  | F32
  | F64
  | Pointer -- an I32 really
  | Void
  deriving stock (Eq, Ord, Show)

data WasmMemory = WasmMemory
  { wmeMemoryStart :: Natural,
    wmeImport :: Maybe (T.Text, T.Text)
  }
  deriving stock (Eq, Ord, Show)

data WasmGlobal = WasmGlobal
  {wgExpr :: WasmExpr, wgType :: WasmType, wgMutable :: Bool}
  deriving stock (Eq, Ord, Show)

data WasmModule = WasmModule
  { -- | the functions themselves, their index comes from the list placement
    wmFunctions :: [WasmFunction],
    -- | the imports, their index comes from placement, after the functions
    wmImports :: [WasmImport],
    -- | where should memory allocation start?
    wmMemory :: WasmMemory,
    -- | which globals are defined?
    wmGlobals :: [WasmGlobal]
  }
  deriving stock (Eq, Ord, Show)

data WasmFunction = WasmFunction
  { wfName :: FunctionName,
    wfExpr :: WasmExpr,
    wfPublic :: Bool,
    wfArgs :: [WasmType],
    wfReturnType :: WasmType,
    wfLocals :: [WasmType]
  }
  deriving stock (Eq, Ord, Show)

data WasmImport = WasmImport
  { wiName :: FunctionName,
    wiArgs :: [WasmType],
    wiReturnType :: WasmType,
    wiExternalModule :: T.Text,
    wiExternalFunction :: T.Text
  }
  deriving stock (Eq, Ord, Show)

data WasmPrim
  = WPInt32 Word32
  | WPInt64 Word64
  | WPFloat32 Float
  | WPFloat64 Double
  | WPBool Bool
  deriving stock (Eq, Ord, Show)

data WasmExpr
  = WPrim WasmPrim
  | WInfix WasmType Op WasmExpr WasmExpr
  | WLet Natural WasmExpr WasmExpr
  | WSequence WasmType WasmExpr WasmExpr -- first type, do first, return second
  | WIf WasmType WasmExpr WasmExpr WasmExpr -- return type, pred, then, else
  | WVar Natural
  | WGlobal Natural
  | WApply Natural [WasmExpr]
  | WAllocate Natural Natural -- function number, size of allocation
  | WSet Natural WasmExpr [(Natural, WasmType, WasmExpr)] -- `(1,2)` is WSet 3 (WAllocate 16) [(0, Int32, 1),(1, Int32, 2)]
  | WTupleAccess WasmType WasmExpr Natural
  | WLoad WasmType Natural -- unsafe load from linear memory
  | WStore WasmType Natural WasmExpr -- unsafe store from linear memory
  | WGlobalSet Natural WasmExpr -- set global value
  deriving stock (Eq, Ord, Show)
