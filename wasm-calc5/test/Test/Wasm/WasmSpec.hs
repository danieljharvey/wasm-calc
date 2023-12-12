{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.WasmSpec (spec) where

import Calc.Parser
import Calc.Typecheck.Elaborate
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
      Right mod' ->
        case fromModule mod' of
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
          [ ("42", Wasm.VI32 42),
            ("(1 + 1)", Wasm.VI32 2),
            ("1 + 2 + 3 + 4 + 5 + 6", Wasm.VI32 21),
            ("6 * 6", Wasm.VI32 36),
            ("100 - 1", Wasm.VI32 99),
            ("100.0 + 1.0", Wasm.VF64 101.0),
            ("if False then 1 else 2", Wasm.VI32 2),
            ("if 1 == 1 then 7 else 10", Wasm.VI32 7),
            ("if 2 == 1 then True else False", Wasm.VI32 0),
            ( joinLines
                [ "function one() { 1 }",
                  "function two() { 2 }",
                  "one() + two()"
                ],
              Wasm.VI32 3
            ),
            ("function increment(a: Integer) { a + 1 } increment(41)", Wasm.VI32 42),
            ("function sum(a: Integer, b: Integer) { a + b } sum(20,22)", Wasm.VI32 42),
            ("function inc(a: Integer) { a + 1 } inc(inc(inc(inc(0))))", Wasm.VI32 4),
            ( joinLines
                [ "function ignoreTuple(pair: (Integer, Boolean)) { True }",
                  "ignoreTuple((1,True))"
                ],
              Wasm.VI32 1
            ),
            ( joinLines
                [ "(1,True).1"
                ],
              Wasm.VI32 1 -- note we cannot make polymorphic versions of these functions yet, although we will
            ),
            ( joinLines
                [ "function swapIntAndBool(pair: (Integer, Boolean)) { (pair.2, pair.1) }",
                  "function fst(pair: (Boolean, Integer)) { pair.1 }",
                  "fst(swapIntAndBool((1,True)))"
                ],
              Wasm.VI32 1 -- note we cannot make polymorphic versions of these functions yet, although we will
            ),
            ( joinLines
                [ "function fst(pair: (Integer,Integer)) { pair.1 }",
                  "fst(((10,2),(3,4)).1)"
                ],
              Wasm.VI32 10
            )
          ]

    describe "From expressions" $ do
      traverse_ testCompileExpr testVals
