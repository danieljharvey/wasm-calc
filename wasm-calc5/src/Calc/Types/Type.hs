{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Type (Type (..), TypePrim (..), TypeVar (..)) where

import Calc.Types.TypeVar
import qualified Data.List.NonEmpty as NE
import qualified Prettyprinter as PP

data TypePrim = TBool | TInt | TFloat
  deriving stock (Eq, Ord, Show)

instance PP.Pretty TypePrim where
  pretty TBool = "Boolean"
  pretty TInt = "Integer"
  pretty TFloat = "Float"

data Type ann
  = TPrim ann TypePrim
  | TFunction ann [Type ann] (Type ann)
  | TTuple ann (Type ann) (NE.NonEmpty (Type ann))
  | TVar ann TypeVar
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Type ann) where
  pretty (TPrim _ prim) = PP.pretty prim
  pretty (TVar _ var) = PP.pretty var
  pretty (TFunction _ args ret) =
    "(" <> prettyArgs <> ") -> " <> PP.pretty ret
    where
      prettyArgs = PP.concatWith (PP.surround PP.comma) (PP.pretty <$> args)
  pretty (TTuple _ a as) =
    "(" <> PP.cat (PP.punctuate "," (PP.pretty <$> tupleItems a as)) <> ")"
    where
      tupleItems :: a -> NE.NonEmpty a -> [a]
      tupleItems b bs = b : NE.toList bs
