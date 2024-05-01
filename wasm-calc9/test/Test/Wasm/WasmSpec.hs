{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.WasmSpec (spec) where

import           Calc.Dependencies
import           Calc.Linearity            (validateModule)
import           Calc.Parser
import           Calc.Test
import           Calc.Typecheck
import           Calc.Wasm
import qualified Calc.Wasm.FromExpr.Module as FromExpr
import           Calc.Wasm.Run
import qualified Calc.Wasm.ToWasm          as ToWasm
import           Control.Monad.IO.Class
import           Data.Foldable             (traverse_)
import           Data.Hashable             (hash)
import qualified Data.Text                 as T
import           Debug.Trace
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
    Right expr -> case treeShakeModule <$> elaborateModule expr of
      Left typeErr -> error (show typeErr)
      Right typedMod ->
        case validateModule typedMod of
          Left e -> error (show e)
          Right _ ->
            case FromExpr.fromModule typedMod of
              Left e -> error (show e)
              Right wasmMod ->
                ToWasm.moduleToWasm (addAllocCount wasmMod)

-- add a `alloccount` function that returns state of allocator
addAllocCount :: ToWasm.WasmModule -> ToWasm.WasmModule
addAllocCount wasmMod@(ToWasm.WasmModule {ToWasm.wmFunctions}) =
  let testFuncIndex =
        ToWasm.WasmFunctionRef (fromIntegral $ length wmFunctions - 1)
      testFuncReturnType =
        ToWasm.wfReturnType (last wmFunctions)
      expr = case ToWasm.moduleUsesAllocator wasmMod of
        ToWasm.UsesAllocator ->
          ToWasm.WSequence
            testFuncReturnType
            (ToWasm.WApply testFuncIndex mempty)
            ToWasm.WAllocCount
        ToWasm.DoesNotUseAllocator ->
          ToWasm.WPrim (ToWasm.WPInt32 0)

      runAllocFunction =
        ToWasm.WasmFunction
          { ToWasm.wfName = "alloccount",
            ToWasm.wfExpr = traceShowId expr,
            ToWasm.wfPublic = True,
            ToWasm.wfArgs = mempty,
            ToWasm.wfReturnType = ToWasm.I32,
            ToWasm.wfLocals = mempty,
            ToWasm.wfAbilities = mempty
          }
   in wasmMod {ToWasm.wmFunctions = wmFunctions <> [runAllocFunction]}

-- | test using the built-in `wasm` package interpreter
testWithInterpreter :: (T.Text, Wasm.Value) -> Spec
testWithInterpreter (input, result) = it (show input) $ do
  let actualWasmModule = compile input
  resp <- runWasm "test" actualWasmModule
  resp `shouldBe` Just [result]
  -- do we deallocate everything?
  allocResp <- runWasm "alloccount" actualWasmModule
  allocResp `shouldBe` Just [Wasm.VI32 0]

-- | in fear of getting incredibly meta, run the tests from this module
-- using the built-in `wasm` interpreter
runTestsWithInterpreter :: (T.Text, [(T.Text, Bool)]) -> Spec
runTestsWithInterpreter (input, result) = it (show input) $ do
  case parseModuleAndFormatError input of
    Left e -> error (show e)
    Right expr -> case elaborateModule expr of
      Left typeErr -> error (show typeErr)
      Right typedMod -> do
        resp <- testModule typedMod
        resp `shouldBe` result

-- | output actual WASM files for testing
-- test them with node
testWithNode :: (T.Text, T.Text) -> Spec
testWithNode (input, result) = it (show input) $ do
  -- get git root folder, explode it if it fails somehow
  (True, gitRoot) <- getGitRoot
  let actualWasmModule = compile input
      inputHash = hash input
      wasmFilename = gitRoot <> "/wasm-calc9/test/output/" <> show inputHash <> ".wasm"
      jsFilename = gitRoot <> "/wasm-calc9/test/output/test.mjs"
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
                    "export function test() -> Void { if (0: Int32) == 1 then consoleLog(0) else consoleLog(42) }"
                  ],
                "42n"
              ),
              ( joinLines
                  [ "import console.log as consoleLog(number: Int64) -> Void",
                    "export function test() -> Int64 { let _ = consoleLog(42); let _ = consoleLog(42); 100 }"
                  ],
                joinLines ["42n", "42n"]
              ),
              ( joinLines
                  [ "import env.memory as memory 1000",
                    "import console.log as consoleLog(number: Int64) -> Void",
                    "export function test() -> Int64 { let (a,b) = ((1: Int64), (2: Int64)); let _ = consoleLog(a + b); 100 }"
                  ],
                "3n"
              )
            ]

      describe "From module" $ do
        traverse_ testWithNode testVals

    describe "Test with interpreter" $ do
      let asTest str = "export function test() -> Int64 { " <> str <> " }"
      let testVals =
            [ (asTest "42", Wasm.VI64 42),
              (asTest "(1 + 1)", Wasm.VI64 2),
              (asTest "1 + 2 + 3 + 4 + 5 + 6", Wasm.VI64 21),
              (asTest "6 * 6", Wasm.VI64 36),
              (asTest "100 - 1", Wasm.VI64 99),
              (asTest "True && True", Wasm.VI32 1),
              (asTest "True && False", Wasm.VI32 0),
              (asTest "True || False", Wasm.VI32 1),
              (asTest "False || False", Wasm.VI32 0),
              ( "export function test() -> Float64 { 100.0 + 1.0 }",
                Wasm.VF64 101.0
              ),
              (asTest "if False then 1 else (2: Int64)", Wasm.VI64 2),
              ("export function test() -> Int32 { if (1 : Int64) == 1 then 7 else 10 }", Wasm.VI32 7),
              ( "export function test() -> Boolean { if 2 == (1 : Int32) then True else False }",
                Wasm.VI32 0
              ),
              ( asTest "let a = (100: Int64); a + 1",
                Wasm.VI64 101
              ),
              ( asTest "let dog = (1: Int64); let cat = dog + 2; let hat = cat + 3; hat",
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
              ( asTest "let Box(a) = Box((100: Int64)); a",
                Wasm.VI64 100
              ),
              ( asTest "let Box(Box(a)) = Box(Box((100: Int64))); a",
                Wasm.VI64 100
              ),
              ( "export function test() -> Boolean { let (_, b) = ((10 : Int64), True); b }",
                Wasm.VI32 1
              ),
              ( "export function test() -> Boolean { let (a, _) = (True, (10 : Int64)); a }",
                Wasm.VI32 1
              ),
              ( joinLines
                  [ "function fst(pair: (Boolean, Int64)) -> Boolean { let (a, _) = pair; a }",
                    "export function test() -> Boolean { fst((True, 100)) }"
                  ],
                Wasm.VI32 1
              ),
              ( joinLines
                  [ "function swapIntAndBool(pair: (Int64, Boolean)) -> (Boolean, Int64) { let (a,b) = pair; (b,a) }",
                    "function fst(pair: (Boolean, Int64)) -> Boolean { let (a,_) = pair; a }",
                    "export function test() -> Boolean { fst(swapIntAndBool((1,True))) }"
                  ],
                Wasm.VI32 1
              ),
              ( joinLines
                  [ "function sumTuple(pair: (Int64, Int64)) -> Int64 { let (a,b) = pair; a + b }",
                    "export function test() -> Int64 { sumTuple((100,200)) }"
                  ],
                Wasm.VI64 300
              ),
              ( joinLines
                  [ "function sumTuple(pair: (Float64, Float64)) -> Float64 { let (a,b) = pair; a + b }",
                    "export function test() -> Float64 { sumTuple((100.0,200.0)) }"
                  ],
                Wasm.VF64 300.0
              ),
              ( joinLines
                  [ "function fst<a,b>(pair: (a,b)) -> Box(a) { let (a, _) = pair; Box(a) }",
                    asTest "let Box(a) = fst(((10: Int64), (2: Int64))); a"
                  ],
                Wasm.VI64 10
              ),
              ( joinLines
                  [ asTest "let (Box(a),_) = (Box((43 : Int64)),Box((42 : Int64))); a"
                  ],
                Wasm.VI64 43
              ),
              ( joinLines
                  [ asTest "let Box(a) = Box((42 : Int64)); let Box(b) = Box((41 : Int64)); a + b"
                  ],
                Wasm.VI64 83
              ),
              ( joinLines
                  [ asTest "let Box(Box(a)) = Box(Box((41 : Int64))); a"
                  ],
                Wasm.VI64 41
              ),
              ( joinLines
                  [ asTest "let (a, (b, c)) = ((1: Int64), ((2: Int64), (3: Int64))); a + b + c"
                  ],
                Wasm.VI64 6
              ),
              ( joinLines
                  [ "function pair<a,b>(left: a, right:b) -> (a,b) { (left, right) }",
                    asTest "let (Box(a),_) = pair(Box((43 : Int64)),Box((42 : Int64))); a"
                  ],
                Wasm.VI64 43
              ),
              ( joinLines
                  [ "function pair<a,b>(left: a, right: b) -> (a,b) { (left, right) }",
                    asTest "let (_, Box(a)) = pair(Box((43 : Int64)),Box((42 : Int64))); a"
                  ],
                Wasm.VI64 42
              ),
              ( asTest "let _ = (1: Int64); (2 : Int64)",
                Wasm.VI64 2
              ),
              ( asTest "let Box(a) = Box((42 : Int64)); a",
                Wasm.VI64 42
              ),
              ( asTest "let Box(a) = Box((1.23: Float32)); let _ = a; (23 : Int64)",
                Wasm.VI64 23
              ),
              ( asTest "let (a,b) = ((1: Int64), (2 : Int64)); a + b",
                Wasm.VI64 3
              ),
              ( asTest "let Box(Box(a)) = Box(Box((101 : Int64))); a",
                Wasm.VI64 101
              ),
              ( asTest "let (a, (b,c)) = ((1 : Int64), ((2: Int64), (3: Int64))); a + b + c",
                Wasm.VI64 6
              ),
              ( asTest "let (_,_,_,d) = ((1: Int8), (2: Int16), (3: Int32), (4: Int64)); d",
                Wasm.VI64 4
              ),
              ( joinLines
                  [ "memory 1000",
                    asTest "load(0)"
                  ],
                Wasm.VI64 0
              ),
              -- now to make sure our manual memory and allocated memory don't
              -- fuck with one another
              ( joinLines
                  [ "memory 1000",
                    asTest "let pair = ((1: Int64), (2: Int64)); let (a,b) = pair; let _ = a + b; load(32)"
                  ],
                Wasm.VI64 0
              ),
              ( joinLines
                  [ "memory 1000",
                    "function sum(a: Int64, b: Int64) -> Int64 { a + b }",
                    asTest "store(0, (20: Int64)); store(8, (22: Int64)); sum(load(0), load(8))"
                  ],
                Wasm.VI64 42
              ),
              ( joinLines
                  [ "global one: Int64 = 1",
                    asTest "1 + one"
                  ],
                Wasm.VI64 2
              ),
              ( joinLines
                  [ "global mut counter: Int64 = 0",
                    asTest "set(counter, 2); counter"
                  ],
                Wasm.VI64 2
              ),
              ( joinLines
                  [ "function factorial(a: Int64) -> Int64 { if a == 0 then 1 else a * factorial(a - 1) }",
                    asTest "factorial(4)"
                  ],
                Wasm.VI64 24
              ),
              ( joinLines
                  [ "export function testShouldntCollide() -> Int32 { 1 }",
                    asTest "100",
                    "test testShouldntCollide = True"
                  ],
                Wasm.VI64 100
              ),
              ( joinLines
                  [ "function alloc() -> Int64 { let _ = Box((1: Int32)); 22 }",
                    asTest "if True then alloc() else alloc()"
                  ],
                Wasm.VI64 22
              ),
              ( asTest "let a = ((1: Int64), (2: Int64)); let (b,c) = a; b + c",
                Wasm.VI64 3
              )
            ]

      fdescribe "From expressions" $ do
        traverse_ testWithInterpreter testVals

    describe "Run tests" $ do
      let testVals =
            [ ( "test result = True",
                [("result", True)]
              ),
              ( joinLines
                  [ "import my.import as myImport(x: Int64) -> Void",
                    "test dontExplodePlease = True"
                  ],
                [ ( "dontExplodePlease",
                    True
                  )
                ]
              ),
              ( joinLines
                  [ "import my.import as myImport(x: Int64) -> Void",
                    "export function usesImport() -> Void { myImport(100) }",
                    "test dontExplodePlease = True"
                  ],
                [ ( "dontExplodePlease",
                    True
                  )
                ]
              ),
              ( joinLines
                  [ "import my.import as myImport(x: Int64) -> Void",
                    "export function usesImport() -> Void { myImport(100) }",
                    "function returnTrue() -> Boolean { True }",
                    "test dontExplodePlease = { returnTrue() }"
                  ],
                [ ( "dontExplodePlease",
                    True
                  )
                ]
              )
            ]

      describe "From tests" $ do
        traverse_ runTestsWithInterpreter testVals
