{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Identifier
  ( identifierParser,
    functionNameParser,
    typeVarParser,
  )
where

import           Calc.Parser.Shared
import           Calc.Parser.Types
import           Calc.Types
import           Control.Monad
import qualified Data.Char          as Char
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)
import           Text.Megaparsec

protectedNames :: Set Text
protectedNames =
  S.fromList
    [ "if",
      "then",
      "else",
      "function"
    ]

filterProtectedNames :: Text -> Maybe Text
filterProtectedNames tx =
  if S.member tx protectedNames
    then Nothing
    else Just tx

protectedTypeNames :: Set Text
protectedTypeNames =
  S.fromList
    [ "Integer",
      "Float",
      "Boolean", "Unit", "Box"
    ]

filterProtectedTypeNames :: Text -> Maybe Text
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
