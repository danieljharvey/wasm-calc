{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Global (Global (..), Mutability (..)) where

import Calc.Types.Expr
import Calc.Types.Identifier

data Mutability = Constant | Mutable
  deriving stock (Eq, Ord, Show)

data Global ann = Global
  { glbAnn :: ann,
    glbMutability :: Mutability,
    glbIdentifier :: Identifier,
    glbExpr :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)
