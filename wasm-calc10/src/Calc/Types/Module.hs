{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Types.Module (Module (..), ModuleItem (..)) where

import Calc.Types.Data
import Calc.Types.Function
import Calc.Types.Global
import Calc.Types.Import
import Calc.Types.Memory
import Calc.Types.Test
import Data.Maybe (maybeToList)
import qualified Prettyprinter as PP

-- | in order to parse these items in any order, we parse zero or more items
-- and then turn them into a `Module` later
-- This allows us to pretty print in the original order
data ModuleItem ann
  = ModuleFunction (Function ann)
  | ModuleImport (Import ann)
  | ModuleMemory (Memory ann)
  | ModuleGlobal (Global ann)
  | ModuleTest (Test ann)
  | ModuleData (Data ann)
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (ModuleItem ann) where
  pretty (ModuleFunction func) =
    PP.pretty func
  pretty (ModuleImport imp) =
    PP.pretty imp
  pretty (ModuleMemory mem) = PP.pretty mem
  pretty (ModuleGlobal glob) = PP.pretty glob
  pretty (ModuleTest test) = PP.pretty test
  pretty (ModuleData dt) = PP.pretty dt

data Module ann = Module
  { mdFunctions :: [Function ann],
    mdImports :: [Import ann],
    mdMemory :: Maybe (Memory ann),
    mdGlobals :: [Global ann],
    mdTests :: [Test ann],
    mdDataTypes :: [Data ann]
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Module ann) where
  pretty (Module {mdFunctions, mdImports, mdGlobals, mdTests, mdMemory}) =
    let memory = maybeToList (PP.pretty <$> mdMemory)
        globals = PP.pretty <$> mdGlobals
        imports = PP.pretty <$> mdImports
        functions = PP.pretty <$> mdFunctions
        tests = PP.pretty <$> mdTests
        parts = memory <> globals <> imports <> functions <> tests
     in PP.cat (PP.punctuate PP.line parts)
