{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Calc.Linearity.Types
  ( Linearity (..),
    LinearityType (..),
    LinearState (..),
  )
where

import Calc.Types.Identifier
import qualified Data.Map as M

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

data LinearState ann = LinearState
  { lsVars :: M.Map Identifier (LinearityType, ann),
    lsUses :: [(Identifier, Linearity ann)]
  }
  deriving stock (Eq, Ord, Show, Functor)
