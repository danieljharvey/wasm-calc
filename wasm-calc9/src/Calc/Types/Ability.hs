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
  pretty (AllocateMemory _) = "allocate memory"
  pretty (CallImportedFunction _ fnName) = "call imported function" <+> PP.pretty fnName
  pretty (MutateGlobal _ ident) = "mutate global" <+> PP.pretty ident
