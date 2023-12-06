module Main (main) where

import Test.Hspec
import qualified Test.Interpreter.InterpreterSpec
import qualified Test.Parser.ParserSpec
import qualified Test.Patterns.PatternsSpec
import qualified Test.Typecheck.TypecheckSpec
import qualified Test.Wasm.WasmSpec

main :: IO ()
main = hspec $ do
  Test.Parser.ParserSpec.spec
  Test.Interpreter.InterpreterSpec.spec
  Test.Typecheck.TypecheckSpec.spec
  Test.Patterns.PatternsSpec.spec
  Test.Wasm.WasmSpec.spec
