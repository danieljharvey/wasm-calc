{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Test (testModule) where

import Calc.Dependencies
import Calc.Types
import Calc.Types.ModuleAnnotations
import Calc.Wasm.FromExpr.Module
import Calc.Wasm.Run (runWasm)
import Calc.Wasm.ToWasm.Helpers
import Calc.Wasm.ToWasm.Module
import Calc.Wasm.ToWasm.Types
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Wasm.Interpreter as Wasm

-- | given module, compile it (with tests included) and run them all
testModule :: (Show ann) => Module (Type ann) -> IO [(T.Text, Bool)]
testModule typedMod@(Module {mdTests}) =
  if null mdTests
    then pure mempty
    else case fromModule (removeUnrequiredFunctions typedMod) of
      Left err ->
        -- internal error, explode without grace
        error (show err)
      Right wasmMod -> do
        let wasm = moduleToWasm wasmMod
        traverse
          ( \wt@WasmTest {wtName} -> do
              result <- runWasm (TL.fromStrict (testName wt)) wasm
              case result of
                Just [singleResult] ->
                  if singleResult == Wasm.VI32 1
                    then pure (wtName, True)
                    else pure (wtName, False)
                Just _ -> error "Incorrect number of results returned"
                Nothing -> error "Wasm interpreter failed"
          )
          (wmTests wasmMod)

-- | we won't have the imports to hand in the `wasm` interpreter,
-- so we must remove any functions that use them
-- fortunately, tests cannot use those functions, so we strip anything not
-- mentioned in the tests
removeUnrequiredFunctions :: Module ann -> Module ann
removeUnrequiredFunctions wholeModule =
  let annotatedModule = getModuleDependencies wholeModule
      tests = S.fromList $ DepTest <$> M.keys (maTests annotatedModule)
   in trimDependencies tests annotatedModule wholeModule
