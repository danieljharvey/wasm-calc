{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.Run (runWasm) where

import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Interpreter as Wasm

runWasm :: Wasm.Module -> IO (Maybe [Wasm.Value])
runWasm wasmModule = do
  case Wasm.validate wasmModule of
    Right validModule -> do
      (result, store) <- Wasm.instantiate Wasm.emptyStore mempty validModule
      case result of
        Right moduleInstance ->
          Wasm.invokeExport store moduleInstance "main" mempty
        Left e -> error e
    Left e -> do
      print wasmModule
      error $ "invalid module: " <> show e
