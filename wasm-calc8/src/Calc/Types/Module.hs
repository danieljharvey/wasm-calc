{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Module where

import Calc.Types.Function
import Calc.Types.Import
import GHC.Natural
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

data Module ann = Module
  { mdFunctions :: [Function ann],
    mdImports :: [Import ann],
    mdMemory :: Maybe Natural
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Module ann) where
  pretty (Module {mdFunctions, mdImports, mdMemory}) =
    let memory = case mdMemory of
          Just mem -> ["memory" <+> PP.pretty mem]
          Nothing -> mempty
        imports = PP.pretty <$> mdImports
        functions = PP.pretty <$> mdFunctions
        parts = memory <> imports <> functions
     in PP.cat (PP.punctuate PP.line parts)
