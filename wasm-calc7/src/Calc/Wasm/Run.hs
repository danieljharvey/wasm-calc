{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.Run (runWasm) where

import           Calc.Utils
import qualified Data.Text.Lazy            as TL
import qualified Language.Wasm             as Wasm
import qualified Language.Wasm.Interpreter as Wasm

runWasm :: TL.Text -> Wasm.Module -> IO (Maybe [Wasm.Value])
runWasm startFunctionName wasmModule = do
  case Wasm.validate wasmModule of
    Right validModule -> do
      (result, store) <- Wasm.instantiate Wasm.emptyStore mempty validModule
      case result of
        Right moduleInstance ->
          Wasm.invokeExport store moduleInstance startFunctionName mempty
        Left e -> error $ "Error instantiating wasm module: " <> show e
    Left e ->
      error $
        "invalid module: "
          <> show e
          <> "\n\n"
          <> prettyShow wasmModule
