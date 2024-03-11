{-# LANGUAGE NamedFieldPuns #-}

module Calc.Test where

import Calc.Types
import Calc.Wasm.FromExpr.Module
import Calc.Wasm.Run (runWasm)
import Calc.Wasm.ToWasm.Helpers
import Calc.Wasm.ToWasm.Module
import Calc.Wasm.ToWasm.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Wasm.Interpreter as Wasm

-- | given module, compile it (with tests included) and run them all
testModule :: (Show ann) => Module (Type ann) -> IO [(T.Text, Bool)]
testModule typedMod@(Module {mdTests}) =
  if null mdTests
    then pure mempty
    else case fromModule typedMod of
      Left _ -> pure mempty
      Right wasmMod -> do
        let wasm = moduleToWasm wasmMod
        traverse
          ( \wt@WasmTest {wtName} -> do
              result <- runWasm (TL.fromStrict (testName wt)) wasm
              if result == Just [Wasm.VI32 1]
                then pure (wtName, True)
                else pure (wtName, False)
          )
          (wmTests wasmMod)
