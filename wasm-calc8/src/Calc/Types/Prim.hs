{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Prim
  ( Prim (..),
  )
where

import Data.Word
import qualified Prettyprinter as PP

data Prim
  = PIntLit Word64 -- a polymorphic int literal, we don't know what size
  | PFloatLit Double -- a polymorphic float literal, we don't know what size
  | PBool Bool
  deriving stock (Eq, Ord, Show)

instance PP.Pretty Prim where
  pretty (PIntLit i) = PP.pretty i
  pretty (PFloatLit f) = PP.pretty f
  pretty (PBool b) = PP.pretty b
