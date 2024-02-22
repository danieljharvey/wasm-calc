{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Global (Global (..), Mutability (..)) where

import Calc.Types.Expr
import Calc.Types.Identifier
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

data Mutability = Constant | Mutable
  deriving stock (Eq, Ord, Show)

instance PP.Pretty Mutability where
  pretty Constant = mempty
  pretty Mutable = "mut"

data Global ann = Global
  { glbAnn :: ann,
    glbMutability :: Mutability,
    glbIdentifier :: Identifier,
    glbExpr :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Global ann) where
  pretty (Global {glbMutability, glbIdentifier, glbExpr}) =
    let prettyMut = case glbMutability of
          Constant -> mempty
          Mutable -> PP.space <> PP.pretty glbMutability
        prettyExpr = case glbExpr of
          EAnn _ ty expr -> ":" <+> PP.pretty ty <+> "=" <+> PP.pretty expr
          other -> PP.space <> "=" <+> PP.pretty other
     in "global" <> prettyMut <+> PP.pretty glbIdentifier <> prettyExpr
