{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Module (moduleParser) where

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

globalParser :: Parser (Global Annotation)
globalParser = myLexeme
  $ withLocation
    ( \glbAnn (glbIdentifier, glbExpr) ->
        Global {glbAnn, glbIdentifier, glbExpr, glbMutability = Constant}
    )
  $ do
    stringLiteral "global"
    ident <- identifierParser
    maybeTy <- optional $ stringLiteral ":" >> typeParser
    stringLiteral "="
    expr <- exprParser
    pure
      ( ident,
        case maybeTy of
          Just ty -> EAnn mempty ty expr
          Nothing -> expr
      )

moduleParser :: Parser (Module Annotation)
moduleParser = do
  memory <- optional (try memoryParser)
  globals <- many globalParser
  imports <- many importParser
  funcs <- many functionParser
  pure $
    Module
      { mdFunctions = funcs,
        mdImports = imports,
        mdMemory = memory,
        mdGlobals = globals
      }
