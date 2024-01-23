module Main (main) where

import Test.Hspec
import qualified Test.Linearity.LinearitySpec
import qualified Test.Parser.ParserSpec
import qualified Test.PrettyPrint.PrettyPrintSpec
import qualified Test.Typecheck.TypecheckSpec
import qualified Test.Wasm.WasmSpec

main :: IO ()
main = hspec $ do
  Test.Parser.ParserSpec.spec
  Test.PrettyPrint.PrettyPrintSpec.spec
  Test.Linearity.LinearitySpec.spec
  Test.Typecheck.TypecheckSpec.spec
  Test.Wasm.WasmSpec.spec
