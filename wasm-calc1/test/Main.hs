module Main (main) where

import Test.Hspec
import qualified Test.Interpreter.InterpreterSpec
import qualified Test.Parser.ParserSpec
import qualified Test.Wasm.WasmSpec

main :: IO ()
main = hspec $ do
  Test.Parser.ParserSpec.spec
  Test.Interpreter.InterpreterSpec.spec
  Test.Wasm.WasmSpec.spec
