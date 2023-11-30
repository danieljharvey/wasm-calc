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

data WasmType = I32
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
    wfReturnType :: WasmType
  }
  deriving stock (Eq, Ord, Show)

data WasmExpr
  = WPrim Prim
  | WInfix Op WasmExpr WasmExpr
  | WIf WasmExpr WasmExpr WasmExpr
  | WVar Natural
  | WApply Natural [WasmExpr]
  deriving stock (Eq, Ord, Show)
