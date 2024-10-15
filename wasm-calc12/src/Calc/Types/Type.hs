{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Type (Type (..), TypePrim (..)) where

import Calc.Types.DataName
import Calc.Types.TypeVar
import Calc.Utils
import qualified Data.List.NonEmpty as NE
import GHC.Natural
import qualified Prettyprinter as PP

data TypePrim
  = TBool
  | TInt8
  | TInt16
  | TInt32
  | TInt64
  | TFloat32
  | TFloat64
  | TVoid
  deriving stock (Eq, Ord, Show)

instance PP.Pretty TypePrim where
  pretty TBool = "Boolean"
  pretty TInt8 = "Int8"
  pretty TInt16 = "Int16"
  pretty TInt32 = "Int32"
  pretty TInt64 = "Int64"
  pretty TFloat32 = "Float32"
  pretty TFloat64 = "Float64"
  pretty TVoid = "Void"

-- | resolved types
data Type ann
  = TPrim ann TypePrim
  | TFunction ann [Type ann] (Type ann)
  | TContainer ann (NE.NonEmpty (Type ann))
  | TConstructor ann DataName [Type ann]
  | TVar ann TypeVar
  | TUnificationVar ann Natural
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance PP.Pretty (Type ann) where
  pretty (TPrim _ prim) = PP.pretty prim
  pretty (TVar _ var) = PP.pretty var
  pretty (TFunction _ args ret) =
    "Fn(" <> prettyArgs <> ") -> " <> PP.pretty ret
    where
      prettyArgs = PP.concatWith (PP.surround PP.comma) (PP.pretty <$> args)
  pretty (TUnificationVar _ i) = "U" <> PP.pretty i
  pretty (TContainer _ as)
    | length as == 1 =
        "Box(" <> PP.pretty (NE.head as) <> ")"
  pretty (TContainer _ as) =
    "(" <> PP.cat (PP.punctuate "," (PP.pretty <$> NE.toList as)) <> ")"
  pretty (TConstructor _ dataName []) =
    PP.pretty dataName
  pretty (TConstructor _ dataName args) =
    PP.pretty dataName
      <> "("
      <> PP.group (PP.line' <> indentMulti 2 (PP.cat tyArgs) <> PP.line')
      <> ")"
    where
      tyArgs =
        PP.punctuate ", " (PP.pretty <$> args)
