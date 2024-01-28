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
import           Test.Helpers
import           Test.Hspec
import           Test.RunNode

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
  resp <- runWasm "test" actualWasmModule
  resp `shouldBe` Just [result]

-- | output actual WASM files for testing
-- test them with node
testWithNode :: (T.Text, T.Text) -> Spec
testWithNode (input, result) = it (show input) $ do
  -- get git root folder, explode it if it fails somehow
  (True, gitRoot) <- getGitRoot
  let actualWasmModule = compile input
      inputHash = hash input
      wasmFilename = gitRoot <> "/wasm-calc7/test/output/" <> show inputHash <> ".wasm"
      jsFilename = gitRoot <> "/wasm-calc7/test/output/test.mjs"
  -- write module to a file so we can run it with `wasmtime` etc
  liftIO (writeModule wasmFilename actualWasmModule)
  -- run node js, get output
  (success, output) <- runScriptFromFile wasmFilename jsFilename
  output `shouldBe` T.unpack result
  -- check it succeeded
  success `shouldBe` True

spec :: Spec
spec = do
  describe "WasmSpec" $ do
    describe "Test with node" $ do
      let testVals =
            [ ( joinLines
                  [ "import console.log as consoleLog(number: Int64) -> Void",
                    "export function test() -> Int64 { let _ = consoleLog(42); 100 }"
                  ],
                "42n"
              ),
              ( joinLines
                  [ "import console.log as consoleLog(number: Int64) -> Void",
                    "export function test() -> Int64 { let _ = consoleLog(42); let _ = consoleLog(42); 100 }"
                  ],
                joinLines ["42n", "42n"]
              )
            ]

      describe "From module" $ do
        traverse_ testWithNode testVals

    -- \| we need to find a way of testing these whilst making `main` return
    -- nothing
    describe "Test with interpreter" $ do
      let asTest str = "export function test() -> Int64 { " <> str <> " }"
      let testVals =
            [ (asTest "42", Wasm.VI64 42),
              (asTest "(1 + 1)", Wasm.VI64 2),
              (asTest "1 + 2 + 3 + 4 + 5 + 6", Wasm.VI64 21),
              (asTest "6 * 6", Wasm.VI64 36),
              (asTest "100 - 1", Wasm.VI64 99),
              (asTest "100.0 + 1.0", Wasm.VF64 101.0),
              (asTest "if False then 1 else 2", Wasm.VI64 2),
              (asTest "if 1 == 1 then 7 else 10", Wasm.VI64 7),
              (asTest "if 2 == 1 then True else False", Wasm.VI32 0),
              (asTest "let a = 100; a + 1", Wasm.VI64 101),
              ( asTest "let dog = 1; let cat = dog + 2; let hat = cat + 3; hat",
                Wasm.VI64 6
              ),
              ( joinLines
                  [ "function one() -> Int64 { 1 }",
                    "function two() -> Int64 { 2 }",
                    asTest "one() + two()"
                  ],
                Wasm.VI64 3
              ),
              ( joinLines
                  [ "function increment(a: Int64) -> Int64 { a + 1 }",
                    asTest "increment(41)"
                  ],
                Wasm.VI64 42
              ),
              ( joinLines
                  [ "function sum(a: Int64, b: Int64) -> Int64 { a + b }",
                    asTest "sum(20,22)"
                  ],
                Wasm.VI64 42
              ),
              ( joinLines
                  [ "function inc(a: Int64) -> Int64 { a + 1 }",
                    asTest "inc(inc(inc(inc(0))))"
                  ],
                Wasm.VI64 4
              ),
              ( asTest "Box(100).1",
                Wasm.VI64 100
              ),
              ( asTest "Box(Box(100)).1.1",
                Wasm.VI64 100
              ),
              ( asTest "(10,True).2",
                Wasm.VI32 1
              ),
              ( joinLines
                  [ "function swapIntAndBool(pair: (Int64, Boolean)) -> (Boolean, Int64) { (pair.2, pair.1) }",
                    "function fst(pair: (Boolean, Int64)) -> Boolean { pair.1 }",
                    asTest "fst(swapIntAndBool((1,True)))"
                  ],
                Wasm.VI32 1
              ),
              ( joinLines
                  [ "function sumTuple(pair: (Float64, Float64)) -> Float64 { pair.1 + pair.2 }",
                    asTest "sumTuple((100.0,200.0))"
                  ],
                Wasm.VF64 300.0
              ),
              ( joinLines
                  [ "function fst(pair: (Int64,Int64)) -> Int64 { pair.1 }",
                    asTest "fst(((10,2),(3,4)).1)"
                  ],
                Wasm.VI64 10
              ),
              ( joinLines
                  [ "function fst<a,b>(pair: (a,b)) -> Box(a) { Box(pair.1) }",
                    asTest "fst((10,2)).1"
                  ],
                Wasm.VI64 10
              ),
              ( joinLines
                  [ "function pair<a,b>(left: a, right:b) -> (a,b) { (left, right) }",
                    asTest "pair(Box(43),Box(42)).1.1"
                  ],
                Wasm.VI64 43
              ),
              ( joinLines
                  [ "function pair<a,b>(left: a, right: b) -> (a,b) { (left, right) }",
                    asTest "pair(Box(43),Box(42)).2.1"
                  ],
                Wasm.VI64 42
              ),
              ( asTest "let _ = 1; 2",
                Wasm.VI64 2
              ),
              ( asTest "let Box(a) = Box(42); a",
                Wasm.VI64 42
              ),
              ( asTest "let Box(a) = Box(1.23); let _ = a; 23",
                Wasm.VI64 23
              ),
              ( asTest "let (a,b) = (1,2); a + b",
                Wasm.VI64 3
              ),
              ( asTest "let Box(Box(a)) = Box(Box(101)); a",
                Wasm.VI64 101
              ),
              ( asTest "let (a, (b,c)) = (1, (2,3)); a + b + c",
                Wasm.VI64 6
              )
            ]

      describe "From expressions" $ do
        traverse_ testWithInterpreter testVals
