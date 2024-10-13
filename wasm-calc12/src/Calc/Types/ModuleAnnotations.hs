{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.ModuleAnnotations (ModuleAnnotations (..)) where

import Calc.Types
import qualified Data.Map.Strict as M

data ModuleAnnotations ann = ModuleAnnotations
  { maFunctions :: M.Map FunctionName ann,
    maTests :: M.Map Identifier ann
  }
  deriving stock (Eq, Ord, Show)
