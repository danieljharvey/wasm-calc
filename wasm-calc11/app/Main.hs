module Main where

import qualified Calc
import Control.Applicative
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Options.Applicative as Opt
import System.IO

data AppAction
  = Repl
  | Build Text -- given an input path, turn it into a Wasm module or explode with an error
  | Format Text -- given an input path, format and write new file
  | Lsp -- run the language server

parseAppAction :: Opt.Parser AppAction
parseAppAction =
  Opt.hsubparser
    ( Opt.command
        "repl"
        ( Opt.info
            (pure Repl)
            (Opt.progDesc "Start new calc repl")
        )
        <> Opt.command
          "lsp"
          ( Opt.info
              (pure Lsp)
              (Opt.progDesc "Start calc lsp")
          )
        <> Opt.command
          "build"
          ( Opt.info
              (Build <$> filePathParse)
              (Opt.progDesc "Given a file path, either compile it into a wasm module or return an error")
          )
        <> Opt.command
          "format"
          ( Opt.info
              (Format <$> filePathParse)
              (Opt.progDesc "Given a file path, parse and save a formatted file")
          )
    )

filePathParse :: Opt.Parser Text
filePathParse =
  Opt.argument
    Opt.str
    (Opt.metavar "<file path>")

optionsParse :: Opt.Parser AppAction
optionsParse = parseAppAction

helpfulPreferences :: Opt.ParserPrefs
helpfulPreferences =
  Opt.defaultPrefs
    { Opt.prefShowHelpOnError = True,
      Opt.prefShowHelpOnEmpty = True
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  action <-
    Opt.customExecParser
      helpfulPreferences
      (Opt.info (optionsParse <**> Opt.helper) Opt.fullDesc)
  case action of
    Repl -> Calc.repl
    Build filePath -> Calc.build (T.unpack filePath)
    Format filePath -> Calc.prettyPrint (T.unpack filePath)
    Lsp -> void Calc.lsp
