{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Memory (Memory (..)) where

import Calc.Types.Identifier
import GHC.Natural
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

data Memory ann
  = LocalMemory {lmAnn :: ann, lmLimit :: Natural}
  | ImportedMemory
      { imAnn :: ann,
        imExternalModule :: Identifier,
        imExternalMemoryName :: Identifier,
        imLimit :: Natural
      }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Memory ann) where
  pretty (LocalMemory {lmLimit}) =
    "memory" <+> PP.pretty lmLimit
  pretty (ImportedMemory {imExternalModule, imExternalMemoryName, imLimit}) =
    "import" <+> PP.pretty imExternalModule <> "." <> PP.pretty imExternalMemoryName <+> "as memory" <+> PP.pretty imLimit
