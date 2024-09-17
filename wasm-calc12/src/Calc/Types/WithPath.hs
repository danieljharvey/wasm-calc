{-# LANGUAGE GeneralisedNewtypeDeriving #-}
  {-# LANGUAGE DerivingStrategies #-}
    {-# LANGUAGE OverloadedStrings #-}
module Calc.Types.WithPath (ModulePath(..),WithPath(..)) where

import Calc.Types.ModuleName
import qualified Prettyprinter as PP


newtype ModulePath  = ModulePath [ModuleName]
  deriving newtype (Eq,Ord,Show,Semigroup,Monoid)

data WithPath a = WithPath ModulePath a
  deriving stock (Eq, Ord, Show)

instance (PP.Pretty a) => PP.Pretty (WithPath a) where
  pretty (WithPath (ModulePath path) a) =
    PP.cat $ PP.punctuate ":" ((PP.pretty <$> path) <> [PP.pretty a])

