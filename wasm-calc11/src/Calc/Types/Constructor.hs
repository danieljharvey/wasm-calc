{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Constructor
  ( Constructor (..),
    safeMkConstructor,
    mkConstructor,
  )
where

import qualified Data.Char as Ch
import Data.String
import qualified Data.Text as T
import qualified Prettyprinter as PP

newtype Constructor = Constructor T.Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup
    )

instance PP.Pretty Constructor where
  pretty (Constructor c) = PP.pretty c

instance IsString Constructor where
  fromString = Constructor . T.pack

validConstructor :: T.Text -> Bool
validConstructor a =
  not (T.null a)
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isUpper (T.head a)

mkConstructor :: T.Text -> Constructor
mkConstructor a =
  if validConstructor a
    then Constructor a
    else error $ T.unpack $ "Constructor validation fail for '" <> a <> "'"

safeMkConstructor :: T.Text -> Maybe Constructor
safeMkConstructor a =
  if validConstructor a
    then Just (Constructor a)
    else Nothing
