{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.WasmSpec (spec) where

import           Calc.Linearity            (validateModule)
import           Calc.Parser
import           Calc.Typecheck
import           Calc.Wasm
import           Calc.Wasm.FromExpr
import           Calc.Wasm.Run
import           Calc.Wasm.ToWasm
import           Control.Monad.IO.Class
import           Data.Foldable             (traverse_)
import           Data.Hashable             (hash)
import qualified Data.Text                 as T
import qualified Language.Wasm.Interpreter as Wasm
import qualified Language.Wasm.Structure   as Wasm
import           Test.Hspec

-- | compile module or spit out error
compile :: T.Text -> Wasm.Module
compile input =
  case parseModuleAndFormatError input of
    Left e -> error (show e)
    Right expr -> case elaborateModule expr of
      Left typeErr -> error (show typeErr)
      Right typedMod ->
        case validateModule typedMod of
          Left e -> error (show e)
          Right _ ->
            case fromModule typedMod of
              Left e        -> error (show e)
              Right wasmMod -> moduleToWasm wasmMod

-- | test using the built-in `wasm` package interpreter
testWithInterpreter :: (T.Text, Wasm.Value) -> Spec
testWithInterpreter (input, result) = it (show input) $ do
  let actualWasmModule = compile input
  resp <- runWasm actualWasmModule
  resp `shouldBe` Just [result]

-- | output actual WASM files for testing
-- later this will run them with `wasmtime` and see
-- what is output to stdout etc
-- for now we'll run them locally and cross our fingers
writeTestFiles :: T.Text  -> Spec
writeTestFiles input  = it (show input) $ do
  let actualWasmModule = compile input
      inputHash = hash input
  -- write module to a file so we can run it with `wasmtime` etc
  liftIO (writeModule ("test/output/" <> show inputHash <> ".wasm") actualWasmModule)
  True `shouldBe` True

joinLines :: [T.Text] -> T.Text
joinLines = foldr (\a b -> a <> "\n" <> b) ""

spec :: Spec
spec = do
  describe "WasmSpec" $ do
    fdescribe "Test with wasmtime" $ do
      let testVals =
            [
              joinLines
                ["import console.log as consoleLog(number: Integer) -> Integer",
                  "consoleLog(42)"]
            ]

      describe "From module" $ do
        traverse_ writeTestFiles testVals

    describe "Test with interpreter" $ do
      let testVals =
            [ ("42", Wasm.VI64 42),
              ("(1 + 1)", Wasm.VI64 2),
              ("1 + 2 + 3 + 4 + 5 + 6", Wasm.VI64 21),
              ("6 * 6", Wasm.VI64 36),
              ("100 - 1", Wasm.VI64 99),
              ("100.0 + 1.0", Wasm.VF64 101.0),
              ("if False then 1 else 2", Wasm.VI64 2),
              ("if 1 == 1 then 7 else 10", Wasm.VI64 7),
              ("if 2 == 1 then True else False", Wasm.VI32 0),
              ("let a = 100; a + 1", Wasm.VI64 101),
              ( "let dog = 1; let cat = dog + 2; let hat = cat + 3; hat",
                Wasm.VI64 6
              ),
              ( joinLines
                  [ "function one() { 1 }",
                    "function two() { 2 }",
                    "one() + two()"
                  ],
                Wasm.VI64 3
              ),
              ( "function increment(a: Integer) { a + 1 } increment(41)",
                Wasm.VI64 42
              ),
              ( "function sum(a: Integer, b: Integer) { a + b } sum(20,22)",
                Wasm.VI64 42
              ),
              ( "function inc(a: Integer) { a + 1 } inc(inc(inc(inc(0))))",
                Wasm.VI64 4
              ),
              ( "Box(100).1",
                Wasm.VI64 100
              ),
              ( "Box(Box(100)).1.1",
                Wasm.VI64 100
              ),
              ( "(10,True).2",
                Wasm.VI32 1
              ),
              ( joinLines
                  [ "function swapIntAndBool(pair: (Integer, Boolean)) { (pair.2, pair.1) }",
                    "function fst(pair: (Boolean, Integer)) { pair.1 }",
                    "fst(swapIntAndBool((1,True)))"
                  ],
                Wasm.VI32 1
              ),
              ( joinLines
                  [ "function sumTuple(pair: (Float, Float)) { pair.1 + pair.2 }",
                    "sumTuple((100.0,200.0))"
                  ],
                Wasm.VF64 300.0
              ),
              ( joinLines
                  [ "function fst(pair: (Integer,Integer)) { pair.1 }",
                    "fst(((10,2),(3,4)).1)"
                  ],
                Wasm.VI64 10
              ),
              ( joinLines
                  [ "function fst<a,b>(pair: (a,b)) { Box(pair.1) }",
                    "fst((10,2)).1"
                  ],
                Wasm.VI64 10
              ),
              ( joinLines
                  [ "function pair<a,b>(left: a, right:b) { (left, right) }",
                    "pair(Box(43),Box(42)).1.1"
                  ],
                Wasm.VI64 43
              ),
              ( joinLines
                  [ "function pair<a,b>(left: a, right: b) { (left, right) }",
                    "pair(Box(43),Box(42)).2.1"
                  ],
                Wasm.VI64 42
              ),
              ("let _ = 1; 2", Wasm.VI64 2),
              ("let Box(a) = Box(42); a", Wasm.VI64 42),
              ("let Box(a) = Box(1.23); let _ = a; 23", Wasm.VI64 23),
              ("let (a,b) = (1,2); a + b", Wasm.VI64 3),
              ("let Box(Box(a)) = Box(Box(101)); a", Wasm.VI64 101),
              ("let (a, (b,c)) = (1, (2,3)); a + b + c", Wasm.VI64 6)
            ]

      describe "From expressions" $ do
        traverse_ testWithInterpreter testVals
