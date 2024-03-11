{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Ability where

-- | things that our functions might do
data Ability = AllocateMemory | CallImportedFunction | MutateGlobal
  deriving stock (Eq, Ord, Show)
