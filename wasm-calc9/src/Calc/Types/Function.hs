{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Function
  ( ArgumentName (..),
    FunctionName (..),
    Function (..),
    FunctionArg (..),
  )
where

import Calc.Types.Expr
import Calc.Types.FunctionName
import Calc.Types.Type
import Calc.Types.TypeVar
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc =
  PP.flatAlt (PP.indent (fromIntegral i) doc) doc

newlines :: PP.Doc style -> PP.Doc style
newlines a = PP.line' <> a <> PP.line'

newtype ArgumentName = ArgumentName Text
  deriving newtype (Eq, Ord, Show)

instance IsString ArgumentName where
  fromString = ArgumentName . T.pack

instance PP.Pretty ArgumentName where
  pretty (ArgumentName t) = PP.pretty t

data Function ann = Function
  { fnAnn :: ann,
    fnGenerics :: [TypeVar],
    fnArgs :: [FunctionArg ann],
    fnFunctionName :: FunctionName,
    fnBody :: Expr ann,
    fnPublic :: Bool,
    fnReturnType :: Type ann
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Function ann) where
  pretty (Function {fnReturnType, fnPublic, fnFunctionName, fnArgs, fnBody}) =
    let prettyExport = if fnPublic then "export " else mempty
     in prettyExport
          <> "function"
          <+> ( PP.pretty fnFunctionName
                  <> PP.group
                    ( "("
                        <> newlines
                          ( indentMulti
                              2
                              (PP.cat (PP.punctuate ", " (PP.pretty <$> fnArgs)))
                          )
                    )
                  <> ")"
              )
          <+> "->"
          <+> PP.pretty fnReturnType
          <+> "{"
          <+> PP.group (newlines $ indentMulti 2 (PP.pretty fnBody))
          <> "}"

data FunctionArg ann = FunctionArg {faAnn :: ann, faName :: ArgumentName, faType :: Type ann}
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (FunctionArg ann) where
  pretty (FunctionArg {faName, faType}) =
    PP.pretty faName <> ":" <+> PP.pretty faType
