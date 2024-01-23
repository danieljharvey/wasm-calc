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
  PP.group (PP.flatAlt (PP.indent (fromIntegral i) doc) doc)

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
    fnPublic :: Bool
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Function ann) where
  pretty (Function {fnPublic, fnFunctionName, fnArgs, fnBody}) =
    let prettyExport = if fnPublic then "export " else mempty
     in prettyExport
          <> "function"
          <+> ( PP.pretty fnFunctionName
                  <> "("
                  <> indentMulti 2 (PP.cat (PP.punctuate ", " (PP.pretty <$> fnArgs)))
                  <> ")"
              )
          <+> "{"
          <> PP.line
          <+> indentMulti 2 (PP.pretty fnBody)
          <+> PP.line
          <> "}"

data FunctionArg ann = FunctionArg {faAnn :: ann, faName :: ArgumentName, faType :: Type ann}
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (FunctionArg ann) where
  pretty (FunctionArg {faName, faType}) =
    PP.pretty faName <> ":" <+> PP.pretty faType
