{-# LANGUAGE DerivingStrategies #-}

module Calc.Wasm.Types
  ( WasmType (..),
    WasmModule (..),
    WasmFunction (..),
    WasmExpr (..),
    WasmImport (..),
    FromWasmError (..),
  )
where

import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Prim
import qualified Data.Text as T
import GHC.Natural

data WasmType
  = I32
  | I64
  | F64
  | Pointer -- an I64 really
  | Void
  deriving stock (Eq, Ord, Show)

data WasmModule = WasmModule
  { -- | the functions themselves, their index comes from the list placement
    wmFunctions :: [WasmFunction],
    -- | the imports, their index comes from placement, after the functions
    wmImports :: [WasmImport]
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

data WasmExpr
  = WPrim Prim
  | WInfix WasmType Op WasmExpr WasmExpr
  | WLet Natural WasmExpr WasmExpr
  | WSequence WasmExpr WasmExpr -- do firsr, return second
  | WIf WasmExpr WasmExpr WasmExpr
  | WVar Natural
  | WApply Natural [WasmExpr]
  | WAllocate Natural
  | WSet Natural WasmExpr [(Natural, WasmType, WasmExpr)] -- `(1,2)` is WSet 3 (WAllocate 2) [(0, 1),(1, 2)]
  | WTupleAccess WasmType WasmExpr Natural
  deriving stock (Eq, Ord, Show)

data FromWasmError
  = FunctionTypeNotScalar
  | IdentifierNotFound Identifier
  | FunctionNotFound FunctionName
  deriving stock (Eq, Ord, Show)
