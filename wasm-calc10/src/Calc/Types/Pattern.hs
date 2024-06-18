{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Pattern where

import Calc.Types.Identifier
import qualified Data.List.NonEmpty as NE
import qualified Prettyprinter as PP
import Calc.Types.Prim

data Pattern ann
  = PVar ann Identifier
  | PWildcard ann
  | PTuple ann (Pattern ann) (NE.NonEmpty (Pattern ann))
  | PLiteral ann Prim
  | PBox ann (Pattern ann)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance PP.Pretty (Pattern ann) where
  pretty (PVar _ identifier) = PP.pretty identifier
  pretty (PWildcard _) = "_"
  pretty (PLiteral _ prim) = PP.pretty prim
  pretty (PBox _ inner) = "Box(" <> PP.pretty inner <> ")"
  pretty (PTuple _ a as) =
    "(" <> PP.cat (PP.punctuate "," (PP.pretty <$> tupleItems a as)) <> ")"
    where
      tupleItems :: a -> NE.NonEmpty a -> [a]
      tupleItems b bs = b : NE.toList bs
