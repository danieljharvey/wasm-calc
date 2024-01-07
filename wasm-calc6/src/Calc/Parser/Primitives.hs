{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Primitives
  ( primExprParser,
    primParser,
    intParser,
  )
where

import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Expr
import Calc.Types.Prim
import Control.Applicative
import Data.Functor (($>))
import Text.Megaparsec (try)
import qualified Text.Megaparsec.Char.Lexer as L

----

intParser :: Parser Integer
intParser =
  L.signed (pure ()) L.decimal

---

floatParser :: Parser Float
floatParser =
  L.signed (pure ()) L.float

---

truePrimParser :: Parser Prim
truePrimParser = PBool <$> trueParser

trueParser :: Parser Bool
trueParser = stringLiteral "True" $> True

falsePrimParser :: Parser Prim
falsePrimParser = PBool <$> falseParser

falseParser :: Parser Bool
falseParser = stringLiteral "False" $> False

---

primExprParser :: Parser ParserExpr
primExprParser =
  myLexeme $
    addLocation $
      try (EPrim mempty . PFloat <$> floatParser)
        <|> EPrim mempty . PInt <$> intParser
        <|> EPrim mempty <$> truePrimParser
        <|> EPrim mempty <$> falsePrimParser

----

primParser :: Parser Prim
primParser =
  try (PFloat <$> floatParser)
    <|> PInt <$> intParser
    <|> truePrimParser
    <|> falsePrimParser
