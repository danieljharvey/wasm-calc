{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Primitives
  ( primExprParser,
    primParser,
    word64Parser,
    naturalParser,
  )
where

import           Calc.Parser.Shared
import           Calc.Parser.Types
import           Calc.Types.Expr
import           Calc.Types.Prim
import           Control.Applicative
import           Data.Functor               (($>))
import           Data.Word
import           GHC.Natural
import           Text.Megaparsec            (try)
import qualified Text.Megaparsec.Char.Lexer as L

----

word64Parser :: Parser Word64
word64Parser =
  L.signed (pure ()) L.decimal

---

naturalParser :: Parser Natural
naturalParser = L.decimal

---

doubleParser :: Parser Double
doubleParser =
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
      EPrim mempty <$> primParser

----

primParser :: Parser Prim
primParser =
  try (PFloatLit <$> doubleParser)
    <|> PIntLit <$> word64Parser
    <|> truePrimParser
    <|> falsePrimParser
