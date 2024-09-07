{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Types.Identifier (Identifier (..), safeMkIdentifier) where

import qualified Data.Char as Ch
import Data.Hashable
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Prettyprinter as PP

newtype Identifier = Identifier Text
  deriving newtype (Eq, Ord, Show, Hashable)

instance IsString Identifier where
  fromString = Identifier . T.pack

instance PP.Pretty Identifier where
  pretty (Identifier ident) = PP.pretty ident

validIdentifier :: Text -> Bool
validIdentifier a =
  T.length a > 0
    && T.filter isValidChar a == a
    && not (Ch.isDigit (T.head a))
    && isValidFirstChar (T.head a)

isValidFirstChar :: Char -> Bool
isValidFirstChar a = Ch.isLower a || a == '_'

-- alpha numeric or `_`
isValidChar :: Char -> Bool
isValidChar a = Ch.isAlphaNum a || a == '_'

safeMkIdentifier :: Text -> Maybe Identifier
safeMkIdentifier a =
  if validIdentifier a
    then Just (Identifier a)
    else Nothing
