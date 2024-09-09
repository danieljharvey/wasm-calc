{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Expr (Expr (..)) where

import Calc.Types.Constructor
import Calc.Types.FunctionName
import Calc.Types.Identifier
import Calc.Types.Op
import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import qualified Data.List.NonEmpty as NE
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

data Expr ann
  = EPrim ann Prim
  | ELet ann (Pattern ann) (Expr ann) (Expr ann)
  | EMatch ann (Expr ann) (NE.NonEmpty (Pattern ann, Expr ann))
  | EInfix ann Op (Expr ann) (Expr ann)
  | EIf ann (Expr ann) (Expr ann) (Expr ann)
  | EVar ann Identifier
  | EApply ann FunctionName [Expr ann]
  | ETuple ann (Expr ann) (NE.NonEmpty (Expr ann))
  | EBox ann (Expr ann)
  | EArray ann [Expr ann]
  | EArraySize ann (Expr ann)
  | EArrayStart ann (Expr ann)
  | EConstructor ann Constructor [Expr ann]
  | EAnn ann (Type ann) (Expr ann)
  | ELoad ann (Expr ann) -- index
  | EStore ann (Expr ann) (Expr ann) -- index, value
  | ESet ann Identifier (Expr ann)
  | EBlock ann (Expr ann)
  | EReference ann (Expr ann)
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
  pretty (ELet _ ident (EAnn _ ty body) rest) =
    "let"
      <+> PP.pretty ident
      <> ":"
      <+> PP.pretty ty
      <+> "="
      <+> PP.pretty body
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
  pretty (EMatch _ expr pats) =
    "case"
      <+> PP.pretty expr
      <+> "{"
      <> PP.group
        ( PP.line
            <> indentMulti
              2
              ( PP.cat
                  (PP.punctuate ", " (prettyPat <$> NE.toList pats))
              )
            <+> PP.line'
        )
      <> "}"
    where
      prettyPat (pat, patExpr) =
        PP.pretty pat <+> "->" <+> PP.pretty patExpr
  pretty (EConstructor _ constructor []) =
    PP.pretty constructor
  pretty (EConstructor _ constructor args) =
    PP.pretty constructor
      <> "("
      <> PP.group (PP.line' <> indentMulti 2 (PP.cat pArgs) <> PP.line')
      <> ")"
    where
      pArgs =
        PP.punctuate ", " (PP.pretty <$> args)
  pretty (EInfix _ op a b) =
    PP.pretty a <+> PP.pretty op <+> PP.pretty b
  pretty (EArray _ items) =
    "[" <+> PP.group (PP.line' <>
            indentMulti 2 (PP.cat (PP.punctuate ", " (PP.pretty <$> items))))
            <+> "]"
  pretty (EArraySize _ item) =
    "size(" <> PP.pretty item <> ")"
  pretty (EArrayStart _ item) =
    "start(" <> PP.pretty item <> ")"
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
    "(" <> PP.group (PP.line' <> indentMulti 2 (PP.cat prettyItems) <> PP.line') <> ")"
    where
      prettyItems =
        PP.punctuate ", " (PP.pretty <$> tupleItems a as)

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
  pretty (EBlock _ expr) =
    PP.group ("{" <> PP.line <> indentMulti 2 (PP.pretty expr) <> PP.line <> "}")
  pretty (EReference _ expr) = "&" <> PP.pretty expr
