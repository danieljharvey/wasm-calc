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

import Calc.Build.Print
import Calc.Build.Steps
import Calc.Wasm.Run
import Calc.Wasm.ToWasm.Module
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Wasm.Interpreter as Wasm
import qualified Language.Wasm.Structure as Wasm
import System.Console.Haskeline

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
          case replSteps (T.pack input) of
            Left buildError -> liftIO (printBuildError buildError) >> loop
            Right wasmMod -> do
              resp <- liftIO $ runWasmModule wasmMod
              liftIO $ putStrLn resp
              loop

replSteps ::
  (MonadError BuildError m) =>
  T.Text ->
  m Wasm.Module
replSteps input = do
  parsedModuleItems <-
    liftEither (parseModuleStep (wrapInputInFunction input))

  parsedModule <-
    liftEither (resolveModuleStep parsedModuleItems)

  typedModule <-
    liftEither (typecheckModuleStep input parsedModule)

  liftEither (linearityCheckStep input typedModule)

  _ <- liftEither (abilityCheckStep input parsedModule)

  wasmMod <- liftEither (fromExprStep typedModule)

  pure (moduleToWasm wasmMod)

-- if input does not include `function`, wrap it in
-- `function main() { <input> }`
wrapInputInFunction :: Text -> Text
wrapInputInFunction input =
  if T.isInfixOf "function" input
    then input
    else "function main() { " <> input <> " } "

runWasmModule :: Wasm.Module -> IO String
runWasmModule mod' =
  do
    maybeValues <- runWasm "main" mod'
    case maybeValues of
      Just [Wasm.VI32 i] -> pure $ show i
      Just [Wasm.VI64 i] -> pure $ show i
      Just [Wasm.VF32 f] -> pure $ show f
      Just [Wasm.VF64 f] -> pure $ show f
      other -> error $ "Expected a single return value but got " <> show other
