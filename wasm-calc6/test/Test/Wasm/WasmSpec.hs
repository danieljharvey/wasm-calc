{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.WasmSpec (spec) where

import Calc.Linearity (validateModule)
import Calc.Parser
import Calc.Typecheck
import Calc.Wasm.FromExpr
import Calc.Wasm.Run
import Calc.Wasm.ToWasm
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Language.Wasm.Interpreter as Wasm
import Test.Hspec

testCompileExpr :: (Text, Wasm.Value) -> Spec
testCompileExpr (input, result) = it (show input) $ do
  case parseModuleAndFormatError input of
    Left e -> error (show e)
    Right expr -> case elaborateModule expr of
      Left typeErr -> error (show typeErr)
      Right typedMod ->
        case validateModule typedMod of
          Left e -> error (show e)
          Right _ ->
            case fromModule typedMod of
              Left e -> error (show e)
              Right wasmMod -> do
                resp <- runWasm (moduleToWasm wasmMod)
                resp `shouldBe` Just [result]

joinLines :: [Text] -> Text
joinLines = foldr (\a b -> a <> "\n" <> b) ""

spec :: Spec
spec = do
  describe "WasmSpec" $ do
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
            ("let (a,b) = (1,2); a + b", Wasm.VI64 3),
            ("let Box(a) = Box(42); a", Wasm.VI64 42)
          ]

    describe "From expressions" $ do
      traverse_ testCompileExpr testVals
