{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Module where

import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Import

data Module ann = Module
  { mdFunctions :: [Function ann],
    mdImports :: [Import ann],
    mdExpr :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)
