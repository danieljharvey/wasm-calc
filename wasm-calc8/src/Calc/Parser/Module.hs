module Calc.Parser.Module (moduleParser) where

import Calc.Parser.Function
import Calc.Parser.Import
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Module
import Text.Megaparsec

moduleParser :: Parser (Module Annotation)
moduleParser = do
  imports <- many importParser
  funcs <- many functionParser
  pure $ Module {mdFunctions = funcs, mdImports = imports}
