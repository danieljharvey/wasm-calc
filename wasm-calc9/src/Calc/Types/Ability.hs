{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Ability where

import Calc.Types.Function
import Calc.Types.Identifier

-- | things that our functions might do
data Ability ann
  = AllocateMemory ann
  | CallImportedFunction ann FunctionName
  | MutateGlobal ann Identifier
  deriving stock (Eq, Ord, Show, Functor)
