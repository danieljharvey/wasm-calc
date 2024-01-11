{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Type (Type (..), TypePrim (..)) where

import Calc.Types.TypeVar
import qualified Data.List.NonEmpty as NE
import GHC.Natural
import qualified Prettyprinter as PP

data TypePrim = TBool | TInt | TFloat | TVoid
  deriving stock (Eq, Ord, Show)

instance PP.Pretty TypePrim where
  pretty TBool = "Boolean"
  pretty TInt = "Integer"
  pretty TFloat = "Float"
  pretty TVoid = "Void"

-- | resolved types
data Type ann
  = TPrim ann TypePrim
  | TFunction ann [Type ann] (Type ann)
  | TContainer ann (NE.NonEmpty (Type ann))
  | TVar ann TypeVar
  | TUnificationVar ann Natural
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Type ann) where
  pretty (TPrim _ prim) = PP.pretty prim
  pretty (TVar _ var) = PP.pretty var
  pretty (TFunction _ args ret) =
    "(" <> prettyArgs <> ") -> " <> PP.pretty ret
    where
      prettyArgs = PP.concatWith (PP.surround PP.comma) (PP.pretty <$> args)
  pretty (TUnificationVar _ i) = "U" <> PP.pretty i
  pretty (TContainer _ as)
    | length as == 1 =
        "Box(" <> PP.pretty (NE.head as) <> ")"
  pretty (TContainer _ as) =
    "(" <> PP.cat (PP.punctuate "," (PP.pretty <$> NE.toList as)) <> ")"
