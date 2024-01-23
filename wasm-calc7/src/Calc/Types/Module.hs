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
    let parts = (PP.pretty <$> mdImports) <> (PP.pretty <$> mdFunctions)
     in PP.cat (PP.punctuate PP.line parts)
