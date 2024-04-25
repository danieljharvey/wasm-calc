{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Calc.Wasm.ToWasm.Types
  ( WasmType (..),
    WasmPrim (..),
    WasmModule (..),
    WasmGlobal (..),
    WasmMemory (..),
    WasmFunction (..),
    WasmExpr (..),
    WasmImport (..),
    WasmFunctionRef (..),
    WasmTest (..),
    ToWasmEnv (..),
  )
where

import           Calc.Types.Ability
import           Calc.Types.Function
import           Calc.Types.Op
import qualified Data.Set            as S
import qualified Data.Text           as T
import           Data.Word
import           GHC.Natural

data ToWasmEnv = ToWasmEnv
  { tweImportsOffset   :: Natural,
    tweGlobalOffset    :: Natural,
    tweFunctionsOffset :: Natural
  }

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
    wmeImport      :: Maybe (T.Text, T.Text)
  }
  deriving stock (Eq, Ord, Show)

data WasmGlobal = WasmGlobal
  {wgExpr :: WasmExpr, wgType :: WasmType, wgMutable :: Bool}
  deriving stock (Eq, Ord, Show)

data WasmModule = WasmModule
  { -- | the functions themselves, their index comes from the list placement
    wmFunctions :: [WasmFunction],
    -- | the imports, their index comes from placement, after the functions
    wmImports   :: [WasmImport],
    -- | where should memory allocation start?
    wmMemory    :: WasmMemory,
    -- | which globals are defined?
    wmGlobals   :: [WasmGlobal],
    -- | which tests do we have?
    wmTests     :: [WasmTest]
  }
  deriving stock (Eq, Ord, Show)

data WasmFunction = WasmFunction
  { wfName       :: FunctionName,
    wfExpr       :: WasmExpr,
    wfPublic     :: Bool,
    wfArgs       :: [WasmType],
    wfReturnType :: WasmType,
    wfLocals     :: [WasmType],
    wfAbilities  :: S.Set (Ability ())
  }
  deriving stock (Eq, Ord, Show)

data WasmImport = WasmImport
  { wiName             :: FunctionName,
    wiArgs             :: [WasmType],
    wiReturnType       :: WasmType,
    wiExternalModule   :: T.Text,
    wiExternalFunction :: T.Text
  }
  deriving stock (Eq, Ord, Show)

data WasmTest = WasmTest
  { wtName   :: T.Text,
    wtExpr   :: WasmExpr,
    wtLocals :: [WasmType]
  }
  deriving stock (Eq, Ord, Show)

data WasmPrim
  = WPInt32 Word32
  | WPInt64 Word64
  | WPFloat32 Float
  | WPFloat64 Double
  | WPBool Bool
  deriving stock (Eq, Ord, Show)

data WasmFunctionRef
  = WasmFunctionRef Natural
  | WasmImportRef Natural
  deriving stock (Eq, Ord, Show)

-- drop after instruction
newtype WasmDrop = WasmDrop [Natural]
  deriving newtype (Eq,Ord,Show, Semigroup,Monoid)

data WasmExpr
  = WPrim WasmDrop WasmPrim
  | WInfix WasmDrop WasmType Op WasmExpr WasmExpr
  | WLet WasmDrop Natural WasmExpr WasmExpr
  | WSequence WasmDrop WasmType WasmExpr WasmExpr -- first type, do first, return second
  | WIf WasmDrop WasmType WasmExpr WasmExpr WasmExpr -- return type, pred, then, else
  | WVar WasmDrop Natural
  | WGlobal WasmDrop Natural
  | WApply WasmDrop WasmFunctionRef [WasmExpr]
  | WAllocate WasmDrop Natural -- size of allocation
  | WDrop WasmDrop WasmExpr -- address to drop
  | WAllocCount -- get number of allocations
  | WSet WasmDrop Natural WasmExpr [(Natural, WasmType, WasmExpr)] -- `(1,2)` is WSet 3 (WAllocate 16) [(0, Int32, 1),(1, Int32, 2)]
  | WTupleAccess WasmDrop WasmType WasmExpr Natural
  | WLoad WasmDrop WasmType WasmExpr -- unsafe load from linear memory, index
  | WStore WasmDrop WasmType WasmExpr WasmExpr -- unsafe store from linear memory, index, item
  | WGlobalSet WasmDrop Natural WasmExpr -- set global value
  deriving stock (Eq, Ord, Show)
