{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Types.Data (Data (..), DataName (..), Constructor (..)) where

import Calc.Types.Constructor
import Calc.Types.Identifier
import Calc.Types.Type
import qualified Data.Map.Strict as M

newtype DataName = DataName Constructor
  deriving newtype (Eq, Ord, Show)

data Data ann = Data
  { dtName :: DataName,
    dtVars :: [Identifier],
    dtConstructors :: M.Map Constructor [Type ann]
  }
  deriving stock (Eq, Ord, Show, Functor)
