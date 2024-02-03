{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Types.Module where

import Calc.Types.Function
import Calc.Types.Import
import Data.Maybe (maybeToList)
import GHC.Natural
import qualified Prettyprinter as PP

data Module ann = Module
  { mdFunctions :: [Function ann],
    mdImports :: [Import ann],
    mdMemory :: Maybe Natural
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Module ann) where
  pretty (Module {mdFunctions, mdImports, mdMemory}) =
    let memory = PP.pretty <$> mdMemory
        imports = PP.pretty <$> mdImports
        functions = PP.pretty <$> mdFunctions
        parts = maybeToList memory <> imports <> functions
     in PP.cat (PP.punctuate PP.line parts)
