{-# LANGUAGE DerivingStrategies #-}

module Calc.Wasm.Types
  ( WasmType (..),
    WasmModule (..),
    WasmFunction (..),
    WasmExpr (..),
  )
where

import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Prim
import GHC.Natural

data WasmType
  = I32
  | I64
  | F64
  | Pointer -- an I64 really
  deriving stock (Eq, Ord, Show)

newtype WasmModule = WasmModule
  { -- | the functions themselves, their index comes from the list placement
    wmFunctions :: [WasmFunction]
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

data WasmExpr
  = WPrim Prim
  | WInfix WasmType Op WasmExpr WasmExpr
  | WIf WasmExpr WasmExpr WasmExpr
  | WVar Natural
  | WApply Natural [WasmExpr]
  | WAllocate Natural
  | WSet Natural WasmExpr [(Natural, WasmType, WasmExpr)] -- `(1,2)` is WSet 3 (WAllocate 2) [(0, 1),(1, 2)]
  | WTupleAccess WasmType WasmExpr Natural
  deriving stock (Eq, Ord, Show)
