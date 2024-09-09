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
          <|> constructorParser
          <|> boxParser
          <|> primExprParser
          <|> ifParser
          <|> loadParser
          <|> storeParser
          <|> setParser
          <|> arraySizeParser
          <|> arrayStartParser
          <|> patternMatchParser
          <|> try applyParser
          <|> try varParser
          <|> blockParser
          <|> arrayParser
          <|> inBrackets (addLocation exprParserInternal)
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
    ],
    [ binary "&&" (EInfix mempty OpAnd),
      binary "||" (EInfix mempty OpOr),
      binary "%" (EInfix mempty OpRemainder)
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
  args <- sepEndBy exprParserInternal (stringLiteral ",")
  stringLiteral ")"
  pure (EApply mempty fnName args)

tupleParser :: Parser (Expr Annotation)
tupleParser = label "tuple" $
  addLocation $ do
    _ <- stringLiteral "("
    neArgs <- NE.fromList <$> sepEndBy1 exprParserInternal (stringLiteral ",")
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

arraySizeParser :: Parser (Expr Annotation)
arraySizeParser = label "size" $
  addLocation $ do
    stringLiteral "size"
    stringLiteral "("
    expr <- exprParserInternal
    stringLiteral ")"
    pure $ EArraySize mempty expr

arrayStartParser :: Parser (Expr Annotation)
arrayStartParser = label "start" $
  addLocation $ do
    stringLiteral "start"
    stringLiteral "("
    expr <- exprParserInternal
    stringLiteral ")"
    pure $ EArrayStart mempty expr



constructorParser :: Parser (Expr Annotation)
constructorParser =
  let argsParser = do
        stringLiteral "("
        args <- sepEndBy1 exprParserInternal (stringLiteral ",")
        stringLiteral ")"
        pure args
   in label "constructor" $ addLocation $ do
        constructor <- myLexeme constructorParserInternal
        args <- try argsParser <|> pure mempty
        pure $ EConstructor mempty constructor args

patternMatchParser :: Parser (Expr Annotation)
patternMatchParser = addLocation $ do
  matchExpr <- matchExprWithParser
  stringLiteral "{"
  patterns <- patternMatchesParser
  stringLiteral "}"
  case NE.nonEmpty patterns of
    (Just nePatterns) -> pure $ EMatch mempty matchExpr nePatterns
    _ -> error "need at least one pattern"

matchExprWithParser :: Parser (Expr Annotation)
matchExprWithParser = do
  stringLiteral "case"
  exprParserInternal

patternMatchesParser :: Parser [(Pattern Annotation, Expr Annotation)]
patternMatchesParser = do
  sepEndBy1
    patternCaseParser
    (stringLiteral ",")

patternCaseParser :: Parser (Pattern Annotation, Expr Annotation)
patternCaseParser = do
  pat <- patternParser
  stringLiteral "->"
  patExpr <- exprParserInternal
  pure (pat, patExpr)

arrayParser :: Parser (Expr Annotation)
arrayParser = label "array" $ addLocation $ do
  stringLiteral "["
  args <- sepBy exprParserInternal (stringLiteral ",")
  stringLiteral "]"
  pure (EArray mempty args)

