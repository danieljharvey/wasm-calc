{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
module Calc.Types.Test where

import           Calc.Types.Expr
import           Calc.Types.Identifier
import           Prettyprinter         ((<+>))
import qualified Prettyprinter         as PP


data Test ann = Test
  { tesAnn  :: ann,
    tesName :: Identifier,
    tesExpr :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc =
  PP.flatAlt (PP.indent (fromIntegral i) doc) doc


instance PP.Pretty (Test ann) where
  pretty (Test {tesName,tesExpr})
     = "test" <+> PP.pretty tesName
      <+> "=" <+> PP.line <> indentMulti 2 (PP.pretty tesExpr)
