{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Pattern
  ( patternParser,
  )
where

import Calc.Parser.Primitives
import Calc.Parser.Identifier
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
    ( orInBrackets
        ( try patTupleParser
            <|> try patWildcardParser
            <|> try patVariableParser
            <|> patBoxParser
            <|> patPrimParser
        )
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
    neArgs <- NE.fromList <$> sepBy1 patternParser (stringLiteral ",")
    neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _ -> fail "Expected at least two items in a tuple"
    _ <- stringLiteral ")"
    pure (NE.head neArgs, neTail)

----

patBoxParser :: Parser ParserPattern
patBoxParser = label "box" $
  withLocation PBox $ do
    _ <- stringLiteral "Box"
    _ <- stringLiteral "("
    inner <- patternParser
    _ <- stringLiteral ")"
    pure inner

----

patPrimParser :: Parser ParserPattern
patPrimParser
  = myLexeme $ withLocation PLiteral primParser


