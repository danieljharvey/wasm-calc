{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Types.Data (Data (..), DataName (..), Constructor (..)) where

import Calc.Types.Constructor
import Calc.Types.Type
import Calc.Types.TypeVar
import qualified Data.Map.Strict as M

newtype DataName = DataName Constructor
  deriving newtype (Eq, Ord, Show)

data Data ann = Data
  { dtName :: DataName,
    dtVars :: [TypeVar],
    dtConstructors :: M.Map Constructor [Type ann]
  }
  deriving stock (Eq, Ord, Show, Functor)
