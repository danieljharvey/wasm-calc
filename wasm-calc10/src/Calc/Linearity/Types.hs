{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Calc.Linearity.Types
  ( Linearity (..),
    LinearityType (..),
    LinearState (..),
    UserDefined (..),
  )
where

import Calc.Types.Identifier
import qualified Data.Map as M
import GHC.Natural

-- | Are we using the whole type or bits of it?
-- this distinction will be gone once we can destructure types instead,
-- implicitly destroying them
newtype Linearity ann
  = Whole ann
  deriving newtype (Eq, Ord, Show)
  deriving stock (Functor)

-- | we're less fussy about the use of primitive values
data LinearityType = LTPrimitive | LTBoxed
  deriving stock (Eq, Ord, Show)

-- | differentiate between names provided by a user, and variables
-- created during linearity check to allow us to drop unnamed items
data UserDefined a = UserDefined a | Internal a
  deriving stock (Eq, Ord, Show, Functor)

data LinearState ann = LinearState
  { lsVars :: M.Map (UserDefined Identifier) (LinearityType, ann),
    lsUses :: [(Identifier, Linearity ann)],
    lsFresh :: Natural
  }
  deriving stock (Eq, Ord, Show, Functor)
