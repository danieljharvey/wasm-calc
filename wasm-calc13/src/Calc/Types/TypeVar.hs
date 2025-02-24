{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Types.TypeVar (TypeVar (..), safeMkTypeVar) where

import qualified Data.Char as Ch
import Data.Hashable
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Prettyprinter as PP

newtype TypeVar = TypeVar Text
  deriving newtype (Eq, Ord, Show, Hashable)

instance IsString TypeVar where
  fromString = TypeVar . T.pack

instance PP.Pretty TypeVar where
  pretty (TypeVar ident) = PP.pretty ident

validTypeVar :: Text -> Bool
validTypeVar a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isLower (T.head a)

safeMkTypeVar :: Text -> Maybe TypeVar
safeMkTypeVar a =
  if validTypeVar a
    then Just (TypeVar a)
    else Nothing
