{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Function (functionParser, functionNameParser) where

import Calc.Parser.Expr
import Calc.Parser.Identifier
import Calc.Parser.Shared
import Calc.Parser.Type
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.TypeVar
import Data.Functor (($>))
import qualified Data.Set as S
import Text.Megaparsec

argumentNameParser :: Parser ArgumentName
argumentNameParser = do
  (Identifier fnName) <- identifierParser
  pure (ArgumentName fnName)

exportParser :: Parser Bool
exportParser =
  try (stringLiteral "export" $> True) <|> pure False

functionParser :: Parser (Function Annotation)
functionParser =
  withLocation
    ( \fnAnn (fnPublic, fnFunctionName, fnGenerics, fnArgs, fnReturnType, fnBody, fnAbilityConstraints) ->
        Function
          { fnPublic,
            fnAnn,
            fnArgs,
            fnGenerics,
            fnFunctionName,
            fnReturnType,
            fnBody,
            fnAbilityConstraints
          }
    )
    innerParser
  where
    innerParser = do
      public <- exportParser
      stringLiteral "function"
      abilityConstraints <- try abilityConstraintsParser <|> pure mempty
      fnName <- functionNameParser
      generics <- try genericsParser <|> pure mempty
      stringLiteral "("
      args <- sepBy argTypeParser (stringLiteral ",")
      stringLiteral ")"
      stringLiteral "->"
      returnType <- typeParser
      stringLiteral "{"
      expr <- exprParser
      stringLiteral "}"
      pure
        ( public,
          fnName,
          generics,
          args,
          returnType,
          expr,
          abilityConstraints
        )

abilityConstraintsParser :: Parser (S.Set AbilityConstraint)
abilityConstraintsParser = myLexeme $ do
  let constraintParser =
        (stringLiteral "noallocate" $> NoAllocate)
          <|> (stringLiteral "noglobalmutate" $> NoGlobalMutate)
          <|> (stringLiteral "noimports" $> NoImports)
  stringLiteral "["
  constraints <- many (myLexeme constraintParser)
  stringLiteral "]"
  pure (S.fromList constraints)

genericsParser :: Parser [TypeVar]
genericsParser = do
  stringLiteral "<"
  generics <- sepBy typeVarParser (stringLiteral ",")
  stringLiteral ">"
  pure generics

argTypeParser :: Parser (FunctionArg Annotation)
argTypeParser = withLocation
  ( \faAnn (faName, faType) ->
      FunctionArg {faAnn, faName, faType}
  )
  $ do
    arg <- argumentNameParser
    stringLiteral ":"
    (,) arg <$> typeParser
