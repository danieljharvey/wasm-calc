{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Import (importParser) where

import Calc.Parser.Identifier
import Calc.Parser.Shared
import Calc.Parser.Type
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Import
import Text.Megaparsec

importParser :: Parser (Import Annotation)
importParser =
  withLocation
    ( \impAnn (impExternalModule, impExternalFunction, impImportName, impArgs, impReturnType) ->
        Import {impAnn, impExternalModule, impExternalFunction, impArgs, impImportName, impReturnType}
    )
    innerParser
  where
    innerParser = do
      stringLiteral "import"
      impExternalModule <- identifierParser
      stringLiteral "."
      impExternalFunction <- identifierParser
      stringLiteral "as"
      impName <- functionNameParser
      stringLiteral "("
      args <- sepBy argTypeParser (stringLiteral ",")
      stringLiteral ")"
      stringLiteral "->"
      impReturnType <- typeParser
      pure (impExternalModule, impExternalFunction, impName, args, impReturnType)

argTypeParser :: Parser (ImportArg Annotation)
argTypeParser = withLocation
  ( \iaAnn (iaName, iaType) ->
      ImportArg {iaAnn, iaName, iaType}
  )
  $ do
    arg <- identifierParser
    stringLiteral ":"
    (,) arg <$> typeParser
