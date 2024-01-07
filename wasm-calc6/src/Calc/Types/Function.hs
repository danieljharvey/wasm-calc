{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

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

newtype ArgumentName = ArgumentName Text
  deriving newtype (Eq, Ord, Show)

instance IsString ArgumentName where
  fromString = ArgumentName . T.pack

data Function ann = Function
  { fnAnn :: ann,
    fnGenerics :: [TypeVar],
    fnArgs :: [FunctionArg ann],
    fnFunctionName :: FunctionName,
    fnBody :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)

data FunctionArg ann = FunctionArg {faAnn :: ann, faName :: ArgumentName, faType :: Type ann}
  deriving stock (Eq, Ord, Show, Functor)
