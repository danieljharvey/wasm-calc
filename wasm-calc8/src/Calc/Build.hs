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

import Calc.Linearity
  ( linearityErrorDiagnostic,
    validateModule,
  )
import Calc.Parser
import Calc.Parser.Types
import Calc.PrettyPrint (format)
import Calc.Typecheck
import Calc.Wasm.FromExpr.Expr
import Calc.Wasm.ToWasm.Module
import Calc.Wasm.WriteModule
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import System.Exit

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
        case validateModule typedMod of
          Left linearityError -> do
            printDiagnostic (linearityErrorDiagnostic (T.pack input) linearityError)
              >> pure (ExitFailure 1)
          Right _ ->
            case fromModule typedMod of
              Left fromWasmError -> do
                liftIO (print fromWasmError)
                  >> pure (ExitFailure 1)
              Right wasmMod -> do
                format filePath (T.pack input) parsedModule
                -- print module to stdout
                liftIO $ printModule (moduleToWasm wasmMod)
                pure ExitSuccess

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
