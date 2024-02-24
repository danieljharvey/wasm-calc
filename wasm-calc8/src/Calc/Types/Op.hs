{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Op (Op (..)) where

import qualified Prettyprinter as PP

data Op
  = OpAdd
  | OpMultiply
  | OpSubtract
  | OpEquals
  | OpGreaterThan
  | OpGreaterThanOrEqualTo
  | OpLessThan
  | OpLessThanOrEqualTo
  deriving stock (Eq, Ord, Show)

-- how to print `Op` values
instance PP.Pretty Op where
  pretty OpAdd = "+"
  pretty OpMultiply = "*"
  pretty OpSubtract = "-"
  pretty OpEquals = "=="
  pretty OpGreaterThan = ">"
  pretty OpGreaterThanOrEqualTo = ">="
  pretty OpLessThan = "<"
  pretty OpLessThanOrEqualTo = "<="
