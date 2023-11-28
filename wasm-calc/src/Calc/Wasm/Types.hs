{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Wasm.Types (Type (..), Module (..), Function (..)) where

import Calc.Types.Expr

data Type ann = I32
  deriving stock (Eq, Ord, Show)

data Module ann = Module
  { -- | the functions themselves, their index comes from the list placement
    modFunctions :: [Function ann]
  }
  deriving stock (Eq, Ord, Show)

data Function ann = Function
  { fnName :: String,
    fnExpr :: Expr ann,
    fnPublic :: Bool,
    fnArgs :: [Type ann],
    fnReturnType :: Type ann
  }
  deriving stock (Eq, Ord, Show)
