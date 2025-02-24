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

import Calc.Build.Format (formatAndSave)
import Calc.Build.Steps
import Calc.Test
import Calc.Types.Annotation
import Calc.Types.Module
import Calc.Wasm.ToWasm.Module
import Calc.Wasm.WriteModule
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Error.Diagnose as Diag
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

buildSteps ::
  (MonadIO m, MonadError BuildError m) =>
  T.Text ->
  m ([ModuleItem Annotation], Wasm.Module)
buildSteps input = do
  parsedModuleItems <- liftEither (parseModuleStep input)

  parsedModule <- liftEither (resolveModuleStep parsedModuleItems)

  typedModule <- liftEither (typecheckModuleStep input parsedModule)

  liftEither (linearityCheckStep input typedModule)

  _ <- liftEither (abilityCheckStep input parsedModule)

  testResults <- liftIO (testModule typedModule)

  unless (testsAllPass testResults) $
    throwError (BuildMessage (T.intercalate "\n" (displayResults testResults)))

  wasmMod <- liftEither (fromExprStep typedModule)

  pure (parsedModuleItems, moduleToWasm wasmMod)

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
