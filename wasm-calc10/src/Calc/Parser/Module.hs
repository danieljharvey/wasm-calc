{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Module (moduleItemParser) where

import Calc.Parser.Expr
import Calc.Parser.Function
import Calc.Parser.Identifier
import Calc.Parser.Import
import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Type
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Expr
import Calc.Types.Global
import Calc.Types.Memory
import Calc.Types.Module
import Calc.Types.Test
import Text.Megaparsec

-- `memory 1000`
localMemoryParser :: Parser (Memory Annotation)
localMemoryParser = myLexeme $
  withLocation LocalMemory $ do
    stringLiteral "memory"
    naturalParser

-- `import env.memory as memory 1000`
importMemoryParser :: Parser (Memory Annotation)
importMemoryParser = myLexeme
  $ withLocation
    ( \imAnn (imExternalModule, imExternalMemoryName, imLimit) ->
        ImportedMemory {imAnn, imExternalModule, imExternalMemoryName, imLimit}
    )
  $ do
    stringLiteral "import"
    imExternalModule <- identifierParser
    stringLiteral "."
    imExternalMemoryName <- identifierParser
    stringLiteral "as"
    stringLiteral "memory"
    imLimit <- naturalParser
    pure (imExternalModule, imExternalMemoryName, imLimit)

memoryParser :: Parser (Memory Annotation)
memoryParser = localMemoryParser <|> importMemoryParser

mutabilityParser :: Parser Mutability
mutabilityParser = myLexeme $ do
  maybeMut <- optional (stringLiteral "mut")
  case maybeMut of
    Just _ -> pure Mutable
    Nothing -> pure Constant

-- `global dog = True`
-- `global mut counter: Int32 = 0`
globalParser :: Parser (Global Annotation)
globalParser = myLexeme
  $ withLocation
    ( \glbAnn (glbIdentifier, glbMutability, glbExpr) ->
        Global {glbAnn, glbIdentifier, glbExpr, glbMutability}
    )
  $ do
    stringLiteral "global"
    mutability <- mutabilityParser
    ident <- identifierParser
    maybeTy <- optional $ stringLiteral ":" >> typeParser
    stringLiteral "="
    expr <- exprParser
    pure
      ( ident,
        mutability,
        case maybeTy of
          Just ty -> EAnn mempty ty expr
          Nothing -> expr
      )

-- `test constTrue = True`
testParser :: Parser (Test Annotation)
testParser = myLexeme
  $ withLocation
    ( \tesAnn (tesName, tesExpr) ->
        Test {tesAnn, tesName, tesExpr}
    )
  $ do
    stringLiteral "test"
    tesName <- identifierParser
    stringLiteral "="
    (,) tesName <$> exprParser

moduleItemParser :: Parser (ModuleItem Annotation)
moduleItemParser =
      ModuleFunction
    <$> functionParser
      <|> ModuleGlobal
    <$> globalParser
      <|> ModuleTest
    <$> testParser
      <|> try (ModuleMemory <$> memoryParser)
      <|> ModuleImport
    <$> importParser
