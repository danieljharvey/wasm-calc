module Main where

import qualified Calc
import           Control.Applicative
import           Data.Text           (Text)
import qualified Options.Applicative as Opt
import           System.IO

data AppAction
  = Repl
  | Build Text -- given an input path, turn it into a Wasm module or explode with an error

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
          "build"
          ( Opt.info
              (Build <$> filePathParse)
              (Opt.progDesc "Given a file path, either compile it into a wasm module or return an error")
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
    Repl            -> Calc.repl
    Build _filePath -> undefined -- filePath
