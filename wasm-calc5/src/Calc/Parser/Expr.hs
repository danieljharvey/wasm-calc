{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Expr (exprParser) where

import Calc.Parser.Identifier
import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Expr
import Control.Monad.Combinators.Expr
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import GHC.Natural
import Text.Megaparsec

exprParser :: Parser (Expr Annotation)
exprParser = addLocation (makeExprParser exprPart table) <?> "expression"

exprPart :: Parser (Expr Annotation)
exprPart =
  try tupleAccessParser
    <|> try tupleParser
    <|> boxParser
    <|> inBrackets (addLocation exprParser)
    <|> primExprParser
    <|> ifParser
    <|> try applyParser
    <|> varParser
    <?> "term"

table :: [[Operator Parser (Expr Annotation)]]
table =
  [ [binary "*" (EInfix mempty OpMultiply)],
    [ binary "+" (EInfix mempty OpAdd),
      binary "-" (EInfix mempty OpSubtract)
    ],
    [binary "==" (EInfix mempty OpEquals)]
  ]

binary :: T.Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ stringLiteral name)

ifParser :: Parser (Expr Annotation)
ifParser = addLocation $ do
  stringLiteral "if"
  predExpr <- exprParser
  stringLiteral "then"
  thenExpr <- exprParser
  stringLiteral "else"
  EIf mempty predExpr thenExpr <$> exprParser

varParser :: Parser (Expr Annotation)
varParser = addLocation $ EVar mempty <$> identifierParser

applyParser :: Parser (Expr Annotation)
applyParser = addLocation $ do
  fnName <- functionNameParser
  stringLiteral "("
  args <- sepBy exprParser (stringLiteral ",")
  stringLiteral ")"
  pure (EApply mempty fnName args)

tupleParser :: Parser (Expr Annotation)
tupleParser = label "tuple" $
  addLocation $ do
    _ <- stringLiteral "("
    neArgs <- NE.fromList <$> sepBy1 exprParser (stringLiteral ",")
    neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _ -> fail "Expected at least two items in a tuple"
    _ <- stringLiteral ")"
    pure (ETuple mempty (NE.head neArgs) neTail)

tupleAccessParser :: Parser (Expr Annotation)
tupleAccessParser =
  let natParser :: Parser Natural
      natParser = myLexeme (fromIntegral <$> intParser)

      tupParser :: Parser (Expr Annotation)
      tupParser =
        try tupleParser
          <|> try applyParser
          <|> try varParser
          <|> boxParser
   in label "tuple access" $
        addLocation $ do
          tup <- tupParser
          _ <- stringLiteral "."
          accesses <- sepBy1 natParser (stringLiteral ".")
          pure $
            foldl'
              ( ETupleAccess mempty
              )
              tup
              accesses

boxParser :: Parser (Expr Annotation)
boxParser = label "box" $
  addLocation $ do
    _ <- stringLiteral "Box"
    _ <- stringLiteral "("
    inner <- exprParser
    _ <- stringLiteral ")"
    pure (EBox mempty inner)
