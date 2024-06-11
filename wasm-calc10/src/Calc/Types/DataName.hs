{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.DataName (DataName (..)) where

import Calc.Types.Constructor
import qualified Prettyprinter as PP

newtype DataName = DataName Constructor
  deriving newtype (Eq, Ord, Show, PP.Pretty)
