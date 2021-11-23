{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Mimsa.Types.AST.Identifier where

import qualified Data.Aeson as JSON
import Data.OpenApi (ToSchema)
import GHC.Generics
import Language.Mimsa.Printer

data Identifier var ann
  = Identifier ann var
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

deriving anyclass instance
  (ToSchema var, ToSchema ann, JSON.ToJSONKey var) =>
  ToSchema (Identifier var ann)

instance (Printer var) => Printer (Identifier var ann) where
  prettyDoc (Identifier _ var) = prettyDoc var