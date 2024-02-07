{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Calc.Types.Module (Module(..), Memory(..)) where

import           Calc.Types.Function
import           Calc.Types.Identifier
import           Calc.Types.Import
import           Data.Maybe            (maybeToList)
import           GHC.Natural
import           Prettyprinter         ((<+>))
import qualified Prettyprinter         as PP

data Memory ann =
                LocalMemory { lmAnn :: ann, lmLimit :: Natural }
                | ImportedMemory { imAnn     :: ann,
                    imExternalModule         :: Identifier,
                        imExternalMemoryName :: Identifier,
                        imLimit              :: Natural }
  deriving stock (Eq,Ord,Show,Functor)

instance PP.Pretty (Memory ann) where
  pretty (LocalMemory {lmLimit }) =
      "memory" <+> PP.pretty lmLimit
  pretty (ImportedMemory {imExternalModule, imExternalMemoryName,imLimit})
    = "import" <+> PP.pretty imExternalModule <> "." <> PP.pretty imExternalMemoryName <+> "as memory" <+> PP.pretty imLimit

data Module ann = Module
  { mdFunctions :: [Function ann],
    mdImports   :: [Import ann],
    mdMemory    :: Maybe (Memory ann)
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Module ann) where
  pretty (Module {mdFunctions, mdImports, mdMemory}) =
    let memory = maybeToList (PP.pretty <$> mdMemory)
        imports = PP.pretty <$> mdImports
        functions = PP.pretty <$> mdFunctions
        parts = memory <> imports <> functions
     in PP.cat (PP.punctuate PP.line parts)
