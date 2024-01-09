{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Import
  ( Import (..),
    ImportArg (..),
  )
where

import Calc.Types.FunctionName
import Calc.Types.Identifier
import Calc.Types.Type

data Import ann = Import
  { impAnn :: ann,
    impArgs :: [ImportArg ann],
    impImportName :: FunctionName,
    impReturnType :: Type ann,
    impExternalModule :: Identifier,
    impExternalFunction :: Identifier
  }
  deriving stock (Eq, Ord, Show, Functor)

data ImportArg ann = ImportArg
  { iaAnn :: ann,
    iaName :: Identifier,
    iaType :: Type ann
  }
  deriving stock (Eq, Ord, Show, Functor)
