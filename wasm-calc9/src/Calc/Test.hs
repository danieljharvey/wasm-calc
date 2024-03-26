{-# LANGUAGE NamedFieldPuns #-}

module Calc.Test where

import Calc.Types
import Calc.Wasm.FromExpr.Module
import Calc.Wasm.Run (runWasm)
import Calc.Wasm.ToWasm.Helpers
import Calc.Wasm.ToWasm.Module
import Calc.Wasm.ToWasm.Types
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Wasm.Interpreter as Wasm

-- | given module, compile it (with tests included) and run them all
testModule :: (Show ann) => Module (Type ann) -> IO [(T.Text, Bool)]
testModule typedMod@(Module {mdTests}) =
  if null mdTests
    then pure mempty
    else case fromModule (removeUnrequiredFunctions $ removeImports typedMod) of
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
                _ -> error "Wasm interpreter failed"
          )
          (wmTests wasmMod)

-- | we won't have the imports to hand in the `wasm` interpreter,
-- and we've already checked they're not being used, so strip them out, thanks
removeImports :: Module ann -> Module ann
removeImports wholeMod = wholeMod {mdImports = mempty}

-- | we won't have the imports to hand in the `wasm` interpreter,
-- so we must remove any functions that use them
-- fortunately, tests cannot use those functions, so we strip anything not
-- mentioned in the tests
removeUnrequiredFunctions :: Module ann -> Module ann
removeUnrequiredFunctions wholeMod =
  let getTestDependencies (Test {}) =
        mempty -- need to actually look
      requiredFunctions = foldMap getTestDependencies (mdTests wholeMod)
   in wholeMod {mdFunctions = filter (\Function {fnFunctionName} -> S.member fnFunctionName requiredFunctions) (mdFunctions wholeMod)}
