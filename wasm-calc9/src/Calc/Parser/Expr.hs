{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Expr (exprParser) where

import Calc.Parser.Identifier
import Calc.Parser.Pattern
import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Type
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Expr
import Calc.Types.Op
import Calc.Types.Pattern
import Control.Monad.Combinators.Expr
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
  let parser =
        do
          try annotationParser
          <|> try tupleParser
          <|> boxParser
          <|> inBrackets (addLocation exprParserInternal)
          <|> primExprParser
          <|> ifParser
          <|> loadParser
          <|> storeParser
          <|> setParser
          <|> try applyParser
          <|> try varParser
          <|> blockParser
          <?> "term"
   in addLocation (makeExprParser parser table) <?> "expression"

-- `{ let a = 1; True }`
blockParser :: Parser (Expr Annotation)
blockParser = label "block" $ addLocation $ do
  stringLiteral "{"
  expr <- exprParser
  stringLiteral "}"
  pure $ EBlock mempty expr

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
    maybeTy <- optional $ stringLiteral ":" >> typeParser
    _ <- stringLiteral "="
    expr <- exprParserInternal
    let expr' = case maybeTy of
          Just ty -> EAnn mempty ty expr
          Nothing -> expr
    stringLiteral ";"
    pure (pat, expr')

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

annotationParser :: Parser (Expr Annotation)
annotationParser = label "annotation" $ addLocation $ do
  stringLiteral "("
  expr <- exprParserInternal
  stringLiteral ":"
  ty <- typeParser
  stringLiteral ")"
  pure (EAnn mempty ty expr)

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

boxParser :: Parser (Expr Annotation)
boxParser = label "box" $
  addLocation $ do
    _ <- stringLiteral "Box"
    _ <- stringLiteral "("
    inner <- exprParserInternal
    _ <- stringLiteral ")"
    pure (EBox mempty inner)

loadParser :: Parser (Expr Annotation)
loadParser = label "load" $
  addLocation $ do
    stringLiteral "load"
    stringLiteral "("
    nat <- exprParserInternal
    stringLiteral ")"
    pure $ ELoad mempty nat

storeParser :: Parser (Expr Annotation)
storeParser = label "store" $
  addLocation $ do
    stringLiteral "store"
    stringLiteral "("
    nat <- exprParserInternal
    stringLiteral ","
    expr <- exprParserInternal
    stringLiteral ")"
    pure $ EStore mempty nat expr

setParser :: Parser (Expr Annotation)
setParser = label "set" $
  addLocation $ do
    stringLiteral "set"
    stringLiteral "("
    ident <- identifierParser
    stringLiteral ","
    expr <- exprParserInternal
    stringLiteral ")"
    pure $ ESet mempty ident expr
