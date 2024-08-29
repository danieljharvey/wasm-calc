{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Pattern
  ( patternParser,
  )
where

import Calc.Parser.Identifier
import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Pattern
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec
import Text.Megaparsec.Char

patternParser :: Parser ParserPattern
patternParser =
  label
    "pattern match"
    ( try patWildcardParser
        <|> patPrimParser
        <|> try patVariableParser
        <|> patConstructorParser
        <|> patTupleParser
    )

----

patWildcardParser :: Parser ParserPattern
patWildcardParser =
  myLexeme $
    withLocation
      (\loc _ -> PWildcard loc)
      (string "_")

----

patVariableParser :: Parser ParserPattern
patVariableParser =
  myLexeme $ withLocation PVar identifierParser

----

patTupleParser :: Parser ParserPattern
patTupleParser = label "tuple" $
  withLocation (\loc (pHead, pTail) -> PTuple loc pHead pTail) $ do
    _ <- stringLiteral "("
    neArgs <- NE.fromList <$> sepEndBy1 patternParser (stringLiteral ",")
    neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _ -> fail "Expected at least two items in a tuple"
    _ <- stringLiteral ")"
    pure (NE.head neArgs, neTail)


----

patPrimParser :: Parser ParserPattern
patPrimParser =
  myLexeme $ withLocation PLiteral primParser

----

patArgsParser :: Parser [ParserPattern]
patArgsParser =
  let argsWithBrackets = do
        stringLiteral "("
        args <- sepEndBy1 patternParser (stringLiteral ",")
        stringLiteral ")"
        pure args
   in try argsWithBrackets <|> pure []

patConstructorParser :: Parser ParserPattern
patConstructorParser =
  let parser = do
        cons <- myLexeme constructorParserInternal
        args <- patArgsParser
        pure (cons, args)
   in withLocation
        ( \loc (cons, args) ->
            PConstructor loc cons args
        )
        parser
