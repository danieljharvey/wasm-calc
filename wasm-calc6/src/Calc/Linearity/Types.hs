{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc.Linearity.Types
  ( Linearity (..),
    LinearityType (..),
    LinearState (..),
  )
where

import Calc.Types.Identifier
import qualified Data.Map as M
import GHC.Natural

-- | Are we using the whole type or bits of it?
-- this distinction will be gone once we can destructure types instead,
-- implicitly destroying them
data Linearity
  = Whole
  | Slice Natural
  deriving stock (Eq, Ord, Show)

-- | we're less fussy about the use of primitive values
data LinearityType = LTPrimitive | LTBoxed
  deriving stock (Eq, Ord, Show)

data LinearState ann = LinearState
  { lsVars :: M.Map Identifier (LinearityType, ann),
    lsUses :: [(Identifier, Linearity)]
  }
  deriving stock (Eq, Ord, Show, Functor)
