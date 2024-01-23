{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Import
  ( Import (..),
    ImportArg (..),
  )
where

import Calc.Types.FunctionName
import Calc.Types.Identifier
import Calc.Types.Type
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc =
  PP.flatAlt (PP.indent (fromIntegral i) doc) doc

data Import ann = Import
  { impAnn :: ann,
    impArgs :: [ImportArg ann],
    impImportName :: FunctionName,
    impReturnType :: Type ann,
    impExternalModule :: Identifier,
    impExternalFunction :: Identifier
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Import ann) where
  pretty (Import {impImportName, impReturnType, impExternalModule, impArgs, impExternalFunction}) =
    "import"
      <+> PP.pretty impExternalModule
      <> "."
      <> PP.pretty impExternalFunction
      <+> "as"
      <> PP.line
      <> indentMulti
        2
        ( PP.pretty impImportName
            <> "("
            <> PP.cat (PP.punctuate ", " (PP.pretty <$> impArgs))
            <> ")"
        )
      <+> "->"
      <+> PP.pretty impReturnType

data ImportArg ann = ImportArg
  { iaAnn :: ann,
    iaName :: Identifier,
    iaType :: Type ann
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (ImportArg ann) where
  pretty (ImportArg {iaName, iaType}) =
    PP.pretty iaName <> ":" <+> PP.pretty iaType
