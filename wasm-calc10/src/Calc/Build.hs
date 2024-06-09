{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Calc.Build
  ( build,
  )
where

import Calc.Ability.Check
import Calc.Dependencies
import qualified Calc.Linearity as Linearity
import Calc.Parser
import Calc.Parser.Types
import Calc.PrettyPrint (format)
import Calc.Test
import Calc.Typecheck
import Calc.Wasm.FromExpr.Module
import Calc.Wasm.ToWasm.Module
import Calc.Wasm.WriteModule
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import System.Exit
import System.IO (hPutStrLn)

build :: FilePath -> IO ()
build filePath =
  liftIO $ doBuild filePath >>= exitWith

doBuild :: (MonadIO m) => FilePath -> m ExitCode
doBuild filePath = do
  input <- liftIO (readFile filePath)
  case parseModule (T.pack input) of
    Left bundle ->
      do
        printDiagnostic (fromErrorBundle bundle input)
        >> pure (ExitFailure 1)
    Right parsedModule -> case elaborateModule parsedModule of
      Left typeErr -> do
        printDiagnostic (typeErrorDiagnostic (T.pack input) typeErr)
          >> pure (ExitFailure 1)
      Right typedMod ->
        case Linearity.validateModule typedMod of
          Left linearityError -> do
            printDiagnostic (Linearity.linearityErrorDiagnostic (T.pack input) linearityError)
              >> pure (ExitFailure 1)
          Right _ -> do
            case abilityCheckModule parsedModule of
              Left abilityError ->
                printDiagnostic (abilityErrorDiagnostic (T.pack input) abilityError)
                  >> pure (ExitFailure 1)
              Right _ -> do
                testResults <- liftIO $ testModule typedMod
                if not (testsAllPass testResults)
                  then do
                    printTestResults testResults
                    pure (ExitFailure 1)
                  else case fromModule (treeShakeModule typedMod) of
                    Left fromWasmError -> do
                      liftIO (print fromWasmError)
                        >> pure (ExitFailure 1)
                    Right wasmMod -> do
                      format filePath (T.pack input) parsedModule
                      -- print module to stdout
                      liftIO $ printModule (moduleToWasm wasmMod)
                      pure ExitSuccess

testsAllPass :: [(a, Bool)] -> Bool
testsAllPass = getAll . foldMap (All . snd)

printTestResults :: (MonadIO m) => [(T.Text, Bool)] -> m ()
printTestResults =
  traverse_ printResult
  where
    printResult (name, True) =
      liftIO $ hPutStrLn Diag.stderr $ "✅ " <> show name
    printResult (name, False) =
      liftIO $ hPutStrLn Diag.stderr $ "❌ " <> show name

printDiagnostic :: (MonadIO m) => Diag.Diagnostic Text -> m ()
printDiagnostic =
  Diag.printDiagnostic
    Diag.stderr
    Diag.WithUnicode
    (Diag.TabSize 4)
    Diag.defaultStyle

-- | turn Megaparsec error + input into a Diagnostic
fromErrorBundle :: ParseErrorType -> String -> Diag.Diagnostic Text
fromErrorBundle bundle input =
  let diag =
        errorDiagnosticFromBundle
          Nothing
          "Parse error on input"
          Nothing
          bundle
   in Diag.addFile diag replFilename input
