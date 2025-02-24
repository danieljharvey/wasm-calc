{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Data (dataParser) where

import Calc.Parser.Function (genericsParser)
import Calc.Parser.Identifier
import Calc.Parser.Shared
import Calc.Parser.Type
import Calc.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

dataParser :: Parser (Data Annotation)
dataParser =
  try typeDeclParserWithCons
    <|> typeDeclParserEmpty

---------------------

-- it's your "type Void in ..."
typeDeclParserEmpty :: Parser (Data Annotation)
typeDeclParserEmpty = do
  stringLiteral "type"
  tyName <- dataNameParser
  pure (Data tyName mempty mempty)

-- it's your more complex cases
typeDeclParserWithCons :: Parser (Data Annotation)
typeDeclParserWithCons = do
  stringLiteral "type"
  tyName <- dataNameParser
  tyArgs <- try genericsParser <|> pure mempty
  stringLiteral "="
  Data tyName tyArgs <$> manyTypeConstructors

--------

manyTypeConstructors :: Parser (Map Constructor [Type Annotation])
manyTypeConstructors = do
  tyCons <-
    sepBy
      oneTypeConstructor
      (stringLiteral "|")
  pure (mconcat tyCons)

-----

-- `Dog` or `Left(e)` or `Right(Int32)`
oneTypeConstructor :: Parser (Map Constructor [Type Annotation])
oneTypeConstructor = do
  constructor <- myLexeme constructorParserInternal
  args <- try constructorArgsParser <|> pure mempty
  pure (M.singleton constructor args)

-- `(a,b,Int32)`
constructorArgsParser :: Parser [Type Annotation]
constructorArgsParser = do
  stringLiteral "("
  types <- sepEndBy typeParser (stringLiteral ",")
  stringLiteral ")"
  pure types
