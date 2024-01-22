{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Function (functionParser, functionNameParser) where

import           Calc.Parser.Expr
import           Calc.Parser.Identifier
import           Calc.Parser.Shared
import           Calc.Parser.Type
import           Calc.Parser.Types
import           Calc.Types.Annotation
import           Calc.Types.Function
import           Calc.Types.Identifier
import           Calc.Types.TypeVar
import           Data.Functor           (($>))
import           Text.Megaparsec

argumentNameParser :: Parser ArgumentName
argumentNameParser = do
  (Identifier fnName) <- identifierParser
  pure (ArgumentName fnName)

exportParser :: Parser Bool
exportParser =
  try (stringLiteral "export" $> True) <|> pure False

functionParser :: Parser (Function Annotation)
functionParser =
  withLocation
    ( \fnAnn (fnPublic, fnFunctionName, fnGenerics, fnArgs, fnBody) ->
        Function {fnPublic, fnAnn, fnArgs, fnGenerics, fnFunctionName, fnBody}
    )
    innerParser
  where
    innerParser = do
      public <- exportParser
      stringLiteral "function"
      fnName <- functionNameParser
      generics <- try genericsParser <|> pure mempty
      stringLiteral "("
      args <- sepBy argTypeParser (stringLiteral ",")
      stringLiteral ")"
      stringLiteral "{"
      expr <- exprParser
      stringLiteral "}"
      pure (public, fnName, generics, args, expr)

genericsParser :: Parser [TypeVar]
genericsParser = do
  stringLiteral "<"
  generics <- sepBy typeVarParser (stringLiteral ",")
  stringLiteral ">"
  pure generics

argTypeParser :: Parser (FunctionArg Annotation)
argTypeParser = withLocation
  ( \faAnn (faName, faType) ->
      FunctionArg {faAnn, faName, faType}
  )
  $ do
    arg <- argumentNameParser
    stringLiteral ":"
    (,) arg <$> typeParser
