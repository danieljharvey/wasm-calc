module Calc.Parser.Types
  ( Parser,
    ParseErrorType,
    ParserExpr,
    ParserType,
    ParserFunction,
    ParserModule,
    ParserPattern,
    ParserData,
  )
where

import Calc.Types.Annotation
import Calc.Types.Data
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Module
import Calc.Types.Pattern
import Calc.Types.Type
import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr = Expr Annotation

type ParserType = Type Annotation

type ParserFunction = Function Annotation

type ParserModule = [ModuleItem Annotation]

type ParserPattern = Pattern Annotation

type ParserData = Data Annotation
