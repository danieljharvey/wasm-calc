{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Function (functionParser, functionNameParser) where

import Calc.Parser.Expr
import Calc.Parser.Identifier
import Calc.Parser.Shared
import Calc.Parser.Type
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Type
import Calc.Types.TypeVar
import Text.Megaparsec

argumentNameParser :: Parser ArgumentName
argumentNameParser = do
  (Identifier fnName) <- identifierParser
  pure (ArgumentName fnName)

functionParser :: Parser (Function Annotation)
functionParser =
  withLocation
    ( \fnAnn (fnFunctionName, fnGenerics, fnArgs, fnBody) ->
        Function {fnAnn, fnArgs, fnGenerics, fnFunctionName, fnBody}
    )
    innerParser
  where
    innerParser = do
      stringLiteral "function"
      fnName <- functionNameParser
      generics <- try genericsParser <|> pure mempty
      stringLiteral "("
      args <- sepBy argTypeParser (stringLiteral ",")
      stringLiteral ")"
      stringLiteral "{"
      expr <- exprParser
      stringLiteral "}"
      pure (fnName, generics, args, expr)

genericsParser :: Parser [TypeVar]
genericsParser = do
  stringLiteral "<"
  generics <- sepBy typeVarParser (stringLiteral ",")
  stringLiteral ">"
  pure generics

argTypeParser :: Parser (ArgumentName, Type Annotation)
argTypeParser = do
  arg <- argumentNameParser
  stringLiteral ":"
  (,) arg <$> typeParser
