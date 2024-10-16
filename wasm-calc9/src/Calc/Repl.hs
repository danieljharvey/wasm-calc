{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Calc.Repl
  ( repl,
  )
where

import Calc.Ability.Check
import Calc.Linearity
  ( linearityErrorDiagnostic,
    validateModule,
  )
import Calc.Parser
import Calc.Parser.Types
import Calc.Typecheck
import Calc.Wasm.FromExpr.Module
import Calc.Wasm.Run
import Calc.Wasm.ToWasm.Module
import Calc.Wasm.ToWasm.Types
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import qualified Language.Wasm.Interpreter as Wasm
import System.Console.Haskeline

instance HasHints Void msg where
  hints _ = mempty

repl :: IO ()
repl = do
  putStrLn "Welcome to llvm-calc"
  putStrLn "Exit with :quit"
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          case parseModule (wrapInputInFunction $ T.pack input) of
            Left bundle -> do
              printDiagnostic (fromErrorBundle bundle input)
              loop
            Right parsedModule -> case elaborateModule parsedModule of
              Left typeErr -> do
                printDiagnostic (typeErrorDiagnostic (T.pack input) typeErr)
                loop
              Right typedMod ->
                case validateModule typedMod of
                  Left linearityError -> do
                    printDiagnostic (linearityErrorDiagnostic (T.pack input) linearityError)
                    loop
                  Right _ -> do
                    case abilityCheckModule parsedModule of
                      Left abilityError -> do
                        printDiagnostic (abilityErrorDiagnostic (T.pack input) abilityError)
                        loop
                      Right _ ->
                        case fromModule typedMod of
                          Left _fromWasmError -> do
                            -- printDiagnostic "From Wasm Error"
                            loop
                          Right wasmMod -> do
                            resp <- liftIO $ runWasmModule wasmMod
                            liftIO $ putStrLn resp
                            loop

-- if input does not include `function`, wrap it in
-- `function main() { <input> }`
wrapInputInFunction :: Text -> Text
wrapInputInFunction input =
  if T.isInfixOf "function" input
    then input
    else "function main() { " <> input <> " } "

runWasmModule :: WasmModule -> IO String
runWasmModule mod' =
  do
    maybeValues <- runWasm "main" (moduleToWasm mod')
    case maybeValues of
      Just [Wasm.VI32 i] -> pure $ show i
      Just [Wasm.VI64 i] -> pure $ show i
      Just [Wasm.VF32 f] -> pure $ show f
      Just [Wasm.VF64 f] -> pure $ show f
      other -> error $ "Expected a single return value but got " <> show other

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
