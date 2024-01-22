{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Expr (exprParser) where

import Calc.Parser.Identifier
import Calc.Parser.Pattern
import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Expr
import Calc.Types.Op
import Calc.Types.Pattern
import Control.Monad.Combinators.Expr
import Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Text.Megaparsec

-- | expression, include lets
exprParser :: Parser (Expr Annotation)
exprParser = do
  parts <- try (some functionPartParser) <|> pure mempty
  expr <- exprParserInternal
  pure $
    foldr
      ( \part accumulatedExpr -> case part of
          LetPart ann pat body -> ELet ann pat body accumulatedExpr
          DiscardPart ann body -> ELet ann (PWildcard ann) body accumulatedExpr
      )
      expr
      parts

-- | we should use this rather than `exprParser` internally to stop recursion
-- death
-- it contains everything except `let` bindings
exprParserInternal :: Parser (Expr Annotation)
exprParserInternal =
  let parser = do
        try unboxParser
          <|> try containerAccessParser
          <|> try tupleParser
          <|> boxParser
          <|> inBrackets (addLocation exprParserInternal)
          <|> primExprParser
          <|> ifParser
          <|> try applyParser
          <|> try varParser
          <?> "term"
   in addLocation (makeExprParser parser table) <?> "expression"

data LetPart ann
  = LetPart ann (Pattern ann) (Expr ann)
  | DiscardPart ann (Expr ann)

functionPartParser :: Parser (LetPart Annotation)
functionPartParser = letPartParser <|> try discardPartParser

letPartParser :: Parser (LetPart Annotation)
letPartParser = do
  label "let" $ withLocation (\loc (pat, expr) -> LetPart loc pat expr) $ do
    _ <- stringLiteral "let"
    pat <- patternParser
    _ <- stringLiteral "="
    expr <- exprParserInternal
    stringLiteral ";"
    pure (pat, expr)

discardPartParser :: Parser (LetPart Annotation)
discardPartParser = label "discard" $ withLocation DiscardPart $ do
  expr <- exprParserInternal
  stringLiteral ";"
  pure expr

table :: [[Operator Parser (Expr Annotation)]]
table =
  [ [binary "*" (EInfix mempty OpMultiply)],
    [ binary "+" (EInfix mempty OpAdd),
      binary "-" (EInfix mempty OpSubtract)
    ],
    [binary "==" (EInfix mempty OpEquals)],
    [ binary "<=" (EInfix mempty OpLessThanOrEqualTo),
      binary "<" (EInfix mempty OpLessThan),
      binary ">=" (EInfix mempty OpGreaterThanOrEqualTo),
      binary ">" (EInfix mempty OpGreaterThan)
    ]
  ]

binary :: T.Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ stringLiteral name)

ifParser :: Parser (Expr Annotation)
ifParser = label "if" $ addLocation $ do
  stringLiteral "if"
  predExpr <- exprParserInternal
  stringLiteral "then"
  thenExpr <- exprParserInternal
  stringLiteral "else"
  EIf mempty predExpr thenExpr <$> exprParserInternal

varParser :: Parser (Expr Annotation)
varParser =
  label "var" $
    addLocation $
      EVar mempty <$> identifierParser

applyParser :: Parser (Expr Annotation)
applyParser = addLocation $ do
  fnName <- functionNameParser
  stringLiteral "("
  args <- sepBy exprParserInternal (stringLiteral ",")
  stringLiteral ")"
  pure (EApply mempty fnName args)

tupleParser :: Parser (Expr Annotation)
tupleParser = label "tuple" $
  addLocation $ do
    _ <- stringLiteral "("
    neArgs <- NE.fromList <$> sepBy1 exprParserInternal (stringLiteral ",")
    neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _ -> fail "Expected at least two items in a tuple"
    _ <- stringLiteral ")"
    pure (ETuple mempty (NE.head neArgs) neTail)

unboxParser :: Parser (Expr Annotation)
unboxParser =
  let tupParser :: Parser (Expr Annotation)
      tupParser =
        try containerAccessParser
          <|> try tupleParser
          <|> try applyParser
          <|> try varParser
          <|> boxParser
   in label "unbox" $
        addLocation $ do
          tup <- tupParser
          _ <- stringLiteral "!"
          pure $
            EContainerAccess mempty tup 1

containerAccessParser :: Parser (Expr Annotation)
containerAccessParser =
  let tupParser :: Parser (Expr Annotation)
      tupParser =
        try tupleParser
          <|> try applyParser
          <|> try varParser
          <|> boxParser
   in label "container access" $
        addLocation $ do
          tup <- tupParser
          _ <- stringLiteral "."
          accesses <-
            sepBy1
              (myLexeme naturalParser)
              (stringLiteral ".")
          pure $
            foldl'
              ( EContainerAccess mempty
              )
              tup
              accesses

boxParser :: Parser (Expr Annotation)
boxParser = label "box" $
  addLocation $ do
    _ <- stringLiteral "Box"
    _ <- stringLiteral "("
    inner <- exprParserInternal
    _ <- stringLiteral ")"
    pure (EBox mempty inner)
