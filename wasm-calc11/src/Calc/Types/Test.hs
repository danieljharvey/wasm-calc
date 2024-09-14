{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Test where

import Calc.Types.Expr
import Calc.Types.Identifier
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Calc.Utils

data Test ann = Test
  { tesAnn :: ann,
    tesName :: Identifier,
    tesExpr :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Test ann) where
  pretty (Test {tesName, tesExpr}) =
    "test"
      <+> PP.pretty tesName
      <+> "="
      <+> PP.line
      <> indentMulti 2 (PP.pretty tesExpr)
