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

import Calc.Wasm.FromExpr
import Calc.Wasm.Types
import Calc.Wasm.Run
import Calc.Parser
import Calc.Parser.Types
import Calc.Typecheck.Elaborate
import Calc.Typecheck.Error
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import System.Console.Haskeline
import qualified Language.Wasm.Interpreter as Wasm

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
          case parseModule (T.pack input) of
            Left bundle -> do
              printDiagnostic (fromErrorBundle bundle input)
              loop
            Right expr -> case elaborateModule expr of
              Left typeErr -> do
                printDiagnostic (typeErrorDiagnostic (T.pack input) typeErr)
                loop
              Right typedMod -> case moduleToLLVM typedMod of
                Left llvmErr -> do
                  liftIO $ print llvmErr
                  loop
                Right mod' -> do
                  resp <- liftIO $ runWasmModule mod'
                  liftIO $ putStrLn (T.unpack resp)
                  loop

runWasmModule :: Module ann -> IO String
runWasmModule mod' =
  do
    maybeValues <- runWasm (createModule mod')
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
    True
    True
    4
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
