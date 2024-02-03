{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Module (moduleParser) where

import Calc.Parser.Function
import Calc.Parser.Import
import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Module
import GHC.Natural
import Text.Megaparsec

memoryParser :: Parser Natural
memoryParser = do
  stringLiteral "memory"
  naturalParser

moduleParser :: Parser (Module Annotation)
moduleParser = do
  memory <- try (Just <$> memoryParser) <|> pure Nothing
  imports <- many importParser
  funcs <- many functionParser
  pure $
    Module
      { mdFunctions = funcs,
        mdImports = imports,
        mdMemory = memory
      }
