{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Types.Module where

import Calc.Types.Function
import Calc.Types.Import
import qualified Prettyprinter as PP

data Module ann = Module
  { mdFunctions :: [Function ann],
    mdImports :: [Import ann]
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Module ann) where
  pretty (Module {mdFunctions, mdImports}) =
    let imports = PP.punctuate PP.line (PP.pretty <$> mdImports)
        functions = PP.punctuate PP.line (PP.pretty <$> mdFunctions)
     in PP.cat imports <> PP.line <> PP.cat functions
