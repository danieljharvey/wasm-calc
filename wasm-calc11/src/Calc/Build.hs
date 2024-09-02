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

import Data.Functor (($>))
import qualified Data.Text.IO as T
import Calc.Wasm.ToWasm.Types (WasmModule)
import Calc.Types.Ability
import qualified Data.Set as S
import Control.Monad.Except
import Calc.Types.Type
import Calc.Types.Annotation
import Calc.Types.Module
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
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import qualified Language.Wasm.Structure as Wasm
import System.Exit

build :: FilePath -> IO ()
build filePath =
  liftIO $ doBuild filePath >>= exitWith

doBuild :: (MonadIO m) => FilePath -> m ExitCode
doBuild filePath = do
  input <- liftIO (readFile filePath)

  result <- runExceptT (buildSteps (T.pack input))

  case result of
    Left buildError ->
      printBuildError buildError $> ExitFailure 1
    Right (parsedModuleItems, wasmModule) -> do
      formatAndSave filePath (T.pack input) parsedModuleItems
      -- print module to stdout
      liftIO $ printModule wasmModule
      -- hooray
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

abilityCheckStep :: T.Text -> Module Annotation -> Either BuildError (ModuleAnnotations (S.Set (Ability Annotation)))
abilityCheckStep input parsedModule =
  case abilityCheckModule parsedModule of
    Right a -> pure a
    Left abilityError ->
      throwError $ BuildDiagnostic (abilityErrorDiagnostic input abilityError)

fromExprStep :: Module (Type Annotation) -> Either BuildError WasmModule
fromExprStep typedModule =
  case fromModule (treeShakeModule typedModule) of
    Right wasmMod -> pure wasmMod
    Left fromWasmError -> do
      throwError (BuildMessage $ T.pack $ show fromWasmError)


buildSteps :: (MonadIO m, MonadError BuildError m) => T.Text -> m ([ModuleItem Annotation], Wasm.Module)
buildSteps input = do
  parsedModuleItems <- liftEither (parseModuleStep input)

  parsedModule <- liftEither (resolveModuleStep parsedModuleItems)

  typedModule <- liftEither (typecheckModuleStep input parsedModule)

  liftEither (linearityCheckStep input typedModule)

  _ <- liftEither ( abilityCheckStep input parsedModule)

  testResults <- liftIO (testModule typedModule)

  unless (testsAllPass testResults) $
    throwError (BuildMessage (T.intercalate "\n" (displayResults testResults)))

  wasmMod <- liftEither (fromExprStep typedModule)

  pure (parsedModuleItems,  moduleToWasm wasmMod)

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

printBuildError :: (MonadIO m) => BuildError -> m ()
printBuildError (BuildDiagnostic diag) =
  Diag.printDiagnostic
    Diag.stderr
    Diag.WithUnicode
    (Diag.TabSize 4)
    Diag.defaultStyle
    diag
printBuildError (BuildMessage msg) =
  liftIO (T.hPutStrLn Diag.stderr msg)

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
