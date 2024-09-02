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

import Calc.Types.Type
import Calc.Types.Annotation
import Calc.Types.Module
import Control.Monad.Trans.Class
import Calc.Ability.Check
import Calc.Dependencies
import qualified Calc.Linearity as Linearity
import Calc.Module (resolveModule)
import Calc.Parser
import Calc.Parser.Types
import Calc.PrettyPrint (formatAndSave)
import Calc.Test
import Calc.Typecheck
import Calc.Wasm.FromExpr.Module
import Calc.Wasm.ToWasm.Module
import Calc.Wasm.WriteModule
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import qualified Language.Wasm.Structure as Wasm
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
    Right parsedModuleItems ->
      case resolveModule parsedModuleItems of
        Left err ->
          liftIO (print err)
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
                          formatAndSave filePath (T.pack input) parsedModuleItems
                          -- print module to stdout
                          liftIO $ printModule (moduleToWasm wasmMod)
                          pure ExitSuccess


parseModuleStep :: T.Text -> Either BuildError [ModuleItem Annotation] 
parseModuleStep input = 
    case parseModule input of
      Right a -> pure a
      Left bundle -> throwError $ BuildDiagnostic (fromErrorBundle bundle (T.unpack input))


resolveModuleStep :: [ModuleItem Annotation] -> Either BuildError (Module Annotation)
resolveModuleStep parsedModuleItems = 
  case resolveModule parsedModuleItems of
    Right a -> pure a
    Left err -> throwError $ BuildMessage (T.pack $ show err)

typecheckModuleStep :: T.Text -> Module Annotation -> Either BuildError (Module (Type Annotation))
typecheckModuleStep input parsedModule = 
  case elaborateModule parsedModule of
    Right a -> pure a
    Left typeErr ->
        throwError $ BuildDiagnostic (typeErrorDiagnostic input typeErr)

linearityCheckStep :: T.Text -> Module (Type Annotation) -> Either BuildError ()
linearityCheckStep input typedModule =
  case Linearity.validateModule typedModule of
    Right a -> pure a
    Left linearityError ->
      throwError $ BuildDiagnostic (Linearity.linearityErrorDiagnostic input linearityError)


  {-

buildSteps :: String -> T.Text -> ExceptT BuildError IO Wasm.Module
buildSteps filePath input = do
  parsedModuleItems <-
    parseModule input
      `mapError` \bundle -> BuildDiagnostic (fromErrorBundle bundle (T.unpack input))

  parsedModule <-
    resolveModule parsedModuleItems
      `mapError` \err -> BuildMessage (show err)
  typedMod <-
    elaborateModule parsedModule
      `mapError` \typeErr -> BuildDiagnostic (typeErrorDiagnostic input typeErr)
  _ <-
    Linearity.validateModule typedMod `mapError` \linearityError ->
      BuildDiagnostic (Linearity.linearityErrorDiagnostic input linearityError)
  _ <-
    abilityCheckModule parsedModule `mapError` \abilityError ->
      BuildDiagnostic (abilityErrorDiagnostic input abilityError)
  testResults <- liftIO $ testModule typedMod
  unless (testsAllPass testResults) $
    throwError (BuildMessage (T.intercalate "\n" (displayResults testResults)))
  case fromModule (treeShakeModule typedMod) of
    Left fromWasmError -> do
      liftIO (print fromWasmError)
        >> pure (ExitFailure 1)
    Right wasmMod -> do
      formatAndSave filePath input parsedModuleItems
      -- print module to stdout
      liftIO $ printModule (moduleToWasm wasmMod)
      pure ExitSuccess
-}

data BuildError
  = BuildDiagnostic (Diag.Diagnostic T.Text)
  | BuildMessage Text

testsAllPass :: [(a, Bool)] -> Bool
testsAllPass = getAll . foldMap (All . snd)

displayResults :: [(T.Text, Bool)] -> [T.Text]
displayResults =
  fmap (T.pack . printResult)
  where
    printResult (name, True) =
      "✅ " <> show name
    printResult (name, False) =
      "❌ " <> show name

printTestResults :: (MonadIO m) => [(T.Text, Bool)] -> m ()
printTestResults =
  traverse_ (liftIO . hPutStrLn Diag.stderr . T.unpack) . displayResults

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
