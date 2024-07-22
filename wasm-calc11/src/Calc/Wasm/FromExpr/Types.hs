{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Wasm.FromExpr.Types
  ( FromExprState (..),
    FromExprGlobal (..),
    FromExprFunc (..),
    FromExprImport (..),
    FromWasmError (..),
  )
where

import Calc.Types
import Calc.Wasm.ToWasm.Types
import qualified Data.Map.Strict as M
import GHC.Natural

-- | take our regular module and do the book keeping to get it ready for Wasm
-- town
data FromExprState = FromExprState
  { fesFunctions :: M.Map FunctionName FromExprFunc,
    fesImports :: M.Map FunctionName FromExprImport,
    fesGlobals :: M.Map Identifier FromExprGlobal,
    fesVars :: [(Maybe Identifier, WasmType)],
    fesArgs :: [(Identifier, WasmType)],
    fesGenerated :: [WasmFunction],
    fesDataTypes :: M.Map DataName [Constructor]
  }
  deriving stock (Eq, Ord, Show)

newtype FromExprGlobal = FromExprGlobal
  {fegIndex :: Natural}
  deriving stock (Eq, Ord, Show)

data FromExprFunc = FromExprFunc
  { fefIndex :: Natural,
    fefOriginalArgs :: [Type ()],
    fefArgs :: [WasmType],
    fefReturnType :: WasmType,
    fefGenerics :: [TypeVar]
  }
  deriving stock (Eq, Ord, Show)

newtype FromExprImport = FromExprImport {feiIndex :: Natural}
  deriving newtype (Eq, Ord, Show)

data FromWasmError
  = FunctionTypeNotScalar
  | IdentifierNotFound Identifier
  | FunctionNotFound FunctionName
  | PrimWithNonNumberType Prim (Type ())
  | FunctionAbilityLookupFailed FunctionName
  deriving stock (Eq, Ord, Show)
