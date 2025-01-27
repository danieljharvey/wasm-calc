{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Calc.Linearity.Types
  ( Linearity (..),
    LinearityType (..),
    LinearState (..),
    UserDefined (..),
    Drops (..),
    LinState (..),
  )
where

import Calc.Types.Identifier
import Calc.Types.Type
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Natural

data Drops ann
  = DropIdentifiers (NE.NonEmpty (Identifier, Type ann))
  | DropMe
  deriving stock (Eq, Ord, Show, Functor)

newtype Linearity ann
  = Whole ann
  deriving newtype (Eq, Ord, Show)
  deriving stock (Functor)

-- | we're less fussy about the use of primitive values
data LinearityType = LTPrimitive | LTBoxed
  deriving stock (Eq, Ord, Show)

data LinState ann
  = Fresh ann
  | Used ann
  deriving stock (Eq, Ord, Show, Functor)

-- | differentiate between names provided by a user, and variables
-- created during linearity check to allow us to drop unnamed items
data UserDefined a = UserDefined a | Internal a
  deriving stock (Eq, Ord, Show, Functor)

data LinearState ann = LinearState
  { lsVars :: M.Map (UserDefined Identifier) (LinearityType, ann),
    lsUses :: NE.NonEmpty (M.Map Identifier (LinState ann, Type ann)),
    lsFresh :: Natural,
    lsIgnoreVars :: S.Set Identifier
  }
  deriving stock (Eq, Ord, Show, Functor)
