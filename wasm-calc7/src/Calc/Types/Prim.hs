{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Prim
  ( Prim (..),
  )
where

import           Data.Word
import qualified Prettyprinter as PP

data Prim
  = PInt32 Word32
  | PInt64 Word64
  | PFloat32 Float
  | PFloat64 Double
  | PBool Bool
  deriving stock (Eq, Ord, Show)

instance PP.Pretty Prim where
  pretty (PInt32 i)   = PP.pretty i
  pretty (PInt64 i)   = PP.pretty i
  pretty (PFloat32 f) = PP.pretty f
  pretty (PFloat64 f) = PP.pretty f
  pretty (PBool b)    = PP.pretty b
