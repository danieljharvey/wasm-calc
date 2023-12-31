{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Expr (Expr (..), Op (..)) where

import Calc.Types.Prim
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

data Expr ann
  = EPrim ann Prim
  | EInfix ann Op (Expr ann) (Expr ann)
  | EIf ann (Expr ann) (Expr ann) (Expr ann)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc =
  PP.flatAlt (PP.indent (fromIntegral i) doc) doc

-- | this instance defines how to nicely print `Expr`
instance PP.Pretty (Expr ann) where
  pretty (EPrim _ prim) =
    PP.pretty prim
  pretty (EInfix _ op a b) =
    PP.pretty a <+> PP.pretty op <+> PP.pretty b
  pretty (EIf _ predExpr thenExpr elseExpr) =
    "if" <+> PP.pretty predExpr <+> "then" <+> indentMulti 2 (PP.pretty thenExpr) <+> "else" <+> indentMulti 2 (PP.pretty elseExpr)

data Op
  = OpAdd
  | OpMultiply
  | OpSubtract
  | OpEquals
  deriving stock (Eq, Ord, Show)

-- how to print `Op` values
instance PP.Pretty Op where
  pretty OpAdd = "+"
  pretty OpMultiply = "*"
  pretty OpSubtract = "-"
  pretty OpEquals = "=="
