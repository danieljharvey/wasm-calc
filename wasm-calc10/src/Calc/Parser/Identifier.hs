{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Identifier
  ( identifierParser,
    functionNameParser,
    typeVarParser,
    dataNameParser,
    constructorParserInternal,
  )
where

import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types
import Control.Monad
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Megaparsec

protectedNames :: Set T.Text
protectedNames =
  S.fromList
    [ "if",
      "then",
      "else",
      "function",
      "let",
      "load",
      "store",
      "set"
    ]

filterProtectedNames :: T.Text -> Maybe T.Text
filterProtectedNames tx =
  if S.member tx protectedNames
    then Nothing
    else Just tx

protectedTypeNames :: Set T.Text
protectedTypeNames =
  S.fromList
    [ "Int16",
      "Int32",
      "Int64",
      "Float32",
      "Float64",
      "Boolean",
      "Box"
    ]

filterProtectedTypeNames :: T.Text -> Maybe T.Text
filterProtectedTypeNames tx =
  if S.member tx protectedTypeNames
    then Nothing
    else Just tx

-- identifier
identifierParser :: Parser Identifier
identifierParser =
  myLexeme identifierParserInternal

-- use this when you are going to wrap myLexeme yourself
identifierParserInternal :: Parser Identifier
identifierParserInternal =
  maybePred
    (takeWhile1P (Just "variable name") Char.isAlphaNum)
    (filterProtectedNames >=> safeMkIdentifier)

functionNameParser :: Parser FunctionName
functionNameParser = do
  (Identifier fnName) <- identifierParser
  pure (FunctionName fnName)

-- typeVar
typeVarParser :: Parser TypeVar
typeVarParser =
  myLexeme typeVarParserInternal

-- use this when you are going to wrap myLexeme yourself
typeVarParserInternal :: Parser TypeVar
typeVarParserInternal =
  maybePred
    (takeWhile1P (Just "type variable name") Char.isAlphaNum)
    (filterProtectedTypeNames >=> safeMkTypeVar)

-- `Maybe`, `Either` etc
dataNameParser :: Parser DataName
dataNameParser = myLexeme (DataName <$> constructorParserInternal)

-- use this when you are going to wrap myLexeme yourself
constructorParserInternal :: Parser Constructor
constructorParserInternal =
  maybePred
    constructor
    (filterProtectedNames >=> safeMkConstructor)

constructor :: Parser T.Text
constructor = takeWhile1P (Just "constructor") Char.isAlphaNum
