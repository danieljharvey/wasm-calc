{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Types.Module (Module (..)) where

import Calc.Types.Function
import Calc.Types.Global
import Calc.Types.Import
import Calc.Types.Memory
import Data.Maybe (maybeToList)
import qualified Prettyprinter as PP

data Module ann = Module
  { mdFunctions :: [Function ann],
    mdImports :: [Import ann],
    mdMemory :: Maybe (Memory ann),
    mdGlobals :: [Global ann]
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Module ann) where
  pretty (Module {mdFunctions, mdImports, mdMemory}) =
    let memory = maybeToList (PP.pretty <$> mdMemory)
        imports = PP.pretty <$> mdImports
        functions = PP.pretty <$> mdFunctions
        parts = memory <> imports <> functions
     in PP.cat (PP.punctuate PP.line parts)
