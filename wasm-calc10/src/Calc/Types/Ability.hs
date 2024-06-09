{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Ability where

import Calc.Types.Function
import Calc.Types.Identifier
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

-- | things that our functions might do
data Ability ann
  = AllocateMemory ann
  | CallImportedFunction ann FunctionName
  | MutateGlobal ann Identifier
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Ability ann) where
  pretty (AllocateMemory _) =
    "Allocating memory"
  pretty (CallImportedFunction _ fnName) =
    "Calling imported function" <+> PP.dquotes (PP.pretty fnName)
  pretty (MutateGlobal _ ident) =
    "Mutate global" <+> PP.dquotes (PP.pretty ident)
