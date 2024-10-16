{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Data (Data (..)) where

import Calc.Types.Constructor
import Calc.Types.DataName
import Calc.Types.Type
import Calc.Types.TypeVar
import Calc.Utils
import qualified Data.Map.Strict as M
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

data Data ann = Data
  { dtName :: DataName,
    dtVars :: [TypeVar],
    dtConstructors :: M.Map Constructor [Type ann]
  }
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Data ann) where
  pretty = renderDataType

renderDataType ::
  Data ann ->
  PP.Doc style
renderDataType (Data tyCon vars' constructors') =
  "type"
    <+> PP.pretty tyCon
    <> printVars vars'
    <> if M.null constructors'
      then mempty
      else
        PP.line
          <> indentMulti
            2
            ( PP.align $
                PP.vsep $
                  zipWith
                    (<+>)
                    ("=" : repeat "|")
                    (printCons <$> M.toList constructors')
            )
  where
    printVars [] =
      mempty
    printVars as =
      "<" <> PP.cat (PP.punctuate "," (PP.pretty <$> as)) <> ">"

    printCons (consName, []) =
      PP.pretty consName
    printCons (consName, args) =
      PP.pretty consName
        <> PP.softline'
        <> "("
        <> PP.group
          ( PP.hang
              0
              ( PP.align $
                  PP.vsep (PP.punctuate "," (prettyMt <$> args))
              )
          )
        <> ")"
    prettyMt = PP.pretty
