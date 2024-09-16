{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Types.ModuleName
  ( ModuleName (..),
  )
where

import Data.Hashable
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Prettyprinter as PP

newtype ModuleName = ModuleName Text
  deriving newtype (Eq, Ord, Show, Hashable)

instance IsString ModuleName where
  fromString = ModuleName . T.pack

instance PP.Pretty ModuleName where
  pretty (ModuleName mn) = PP.pretty mn
