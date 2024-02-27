{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Expr (Expr (..)) where

import Calc.Types.FunctionName
import Calc.Types.Identifier
import Calc.Types.Op
import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import qualified Data.List.NonEmpty as NE
import GHC.Natural
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

data Expr ann
  = EPrim ann Prim
  | ELet ann (Pattern ann) (Expr ann) (Expr ann)
  | EInfix ann Op (Expr ann) (Expr ann)
  | EIf ann (Expr ann) (Expr ann) (Expr ann)
  | EVar ann Identifier
  | EApply ann FunctionName [Expr ann]
  | ETuple ann (Expr ann) (NE.NonEmpty (Expr ann))
  | EBox ann (Expr ann)
  | EAnn ann (Type ann) (Expr ann)
  | ELoad ann Natural
  | EStore ann Natural (Expr ann)
  | ESet ann Identifier (Expr ann)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc =
  PP.flatAlt (PP.indent (fromIntegral i) doc) doc

-- | this instance defines how to nicely print `Expr`
instance PP.Pretty (Expr ann) where
  pretty (EPrim _ prim) =
    PP.pretty prim
  pretty (EAnn _ ty expr) =
    PP.parens (PP.pretty expr <> ":" <+> PP.pretty ty)
  pretty (ELet _ (PWildcard _) body rest) =
    PP.pretty body
      <> ";"
      <+> PP.line
      <> PP.pretty rest
  pretty (ELet _ ident body rest) =
    "let"
      <+> PP.pretty ident
      <+> "="
      <+> PP.pretty body
      <> ";"
      <+> PP.line
      <> PP.pretty rest
  pretty (EInfix _ op a b) =
    PP.pretty a <+> PP.pretty op <+> PP.pretty b
  pretty (EIf _ predExpr thenExpr elseExpr) =
    PP.group
      ( "if"
          <+> PP.pretty predExpr
          <+> "then"
          <> PP.line
          <> indentMulti 2 (PP.pretty thenExpr)
          <> PP.line
          <> "else"
          <> PP.line
          <> indentMulti 2 (PP.pretty elseExpr)
      )
  pretty (EVar _ ident) = PP.pretty ident
  pretty (EApply _ fn args) =
    PP.pretty fn
      <> "("
      <> PP.group (PP.line' <> indentMulti 2 (PP.cat pArgs) <> PP.line')
      <> ")"
    where
      pArgs = PP.punctuate ", " (PP.pretty <$> args)
  pretty (ETuple _ a as) =
    "(" <> PP.cat (PP.punctuate "," (PP.pretty <$> tupleItems a as)) <> ")"
    where
      tupleItems :: a -> NE.NonEmpty a -> [a]
      tupleItems b bs = b : NE.toList bs
  pretty (EBox _ inner) =
    "Box(" <> PP.pretty inner <> ")"
  pretty (ELoad _ index) =
    "load(" <> PP.pretty index <> ")"
  pretty (EStore _ index expr) =
    "store(" <> PP.pretty index <> "," <+> PP.pretty expr <> ")"
  pretty (ESet _ ident expr) =
    "set(" <> PP.pretty ident <> "," <+> PP.pretty expr <> ")"
