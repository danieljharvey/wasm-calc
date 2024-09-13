{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Wasm.WasmSpec (spec) where

import Calc.Dependencies
import Calc.Linearity (validateModule)
import Calc.Module
import Calc.Parser
import Calc.Test
import Calc.Typecheck
import Calc.Wasm
import qualified Calc.Wasm.FromExpr.Module as FromExpr
import Calc.Wasm.Run
import qualified Calc.Wasm.ToWasm as ToWasm
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.FileEmbed
import Data.Foldable (traverse_)
import Data.Hashable (hash)
import qualified Data.Text as T
import qualified Language.Wasm.Interpreter as Wasm
import qualified Language.Wasm.Structure as Wasm
import System.IO.Temp
import Test.Helpers
import Test.Hspec
import Test.RunNode

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
                  [ "import console.log as consoleLog(number: Int64) -> Void",
                    "function max(one: Int64, two: Int64) -> Int64 { if one > two then one else two }",
                    "export function test() -> Int64 { let a = max(0,25); let _ = consoleLog(a); let _ = consoleLog(a + 1); 100 }"
                  ],
                joinLines ["25n", "26n"]
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
              (asTest "if True then if True then 1 else 2 else 3", Wasm.VI64 1),
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
                  [ "function fst<a,b>(pair: (a,b)) -> a { let (a, _) = pair; a }",
                    asTest "let one = Box((10: Int64)); let two = Box((20: Int64)); let Box(a) = fst((one, two)); a"
                  ],
                Wasm.VI64 10
              ),
              ( joinLines
                  [ "function drop<a>(item: a) -> Int64 { let _ = item; 100 }",
                    asTest "drop(Box((10: Int64)))"
                  ],
                Wasm.VI64 100
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
              ),
              ( joinLines
                  [ "function bool<a>(pred: Boolean, left: a, right: a) -> a { if pred then left else right }",
                    asTest "let Box(a) = bool(True, Box((1: Int64)), Box((2: Int64))); a"
                  ],
                Wasm.VI64 1
              ),
              ( joinLines
                  [ "function bool(pred: Boolean, left: Box(Int64), right: Box(Int64)) -> Box(Int64) { if pred then left else right }",
                    asTest "let Box(a) = bool(False, Box(1), Box(2)); a"
                  ],
                Wasm.VI64 2
              ),
              ( joinLines
                  [ "function bool(pred: Boolean, left: Box(Box(Int64)), right: Box(Box(Int64))) -> Box(Box(Int64)) { if pred then left else right }",
                    asTest "let Box(Box(a)) = bool(False, Box(Box(1)), Box(Box(2))); a"
                  ],
                Wasm.VI64 2
              ),
              ( joinLines
                  [ "function bool<a>(pred: Boolean, left: a, right: a) -> a { if pred then left else right }",
                    asTest "let Box(a) = bool(False, Box((1: Int64)), Box((2: Int64))); a"
                  ],
                Wasm.VI64 2
              ),
              ( asTest "let a = Box((1: Int64)); let b = Box((2: Int64)); let Box(c) = if True then a else b; c",
                Wasm.VI64 1
              ),
              ( asTest "let Box(Box(Box(a))) = Box(Box(Box((2: Int64)))); a",
                Wasm.VI64 2
              ),
              ( joinLines
                  [ "function drop<a>(a: a) -> Int64 { let _ = a; 100 }",
                    "function useDrop<a>(a: a) -> Int64 { drop(a) }",
                    asTest "let value = Box(Box((1: Int64))); useDrop(value)"
                  ],
                Wasm.VI64 100
              ),
              (asTest "case (100: Int64) { a -> a }", Wasm.VI64 100),
              ( asTest "case True { True -> 1, False -> 2 }",
                Wasm.VI64 1
              ),
              ( asTest "case False { True -> 1, False -> 2 }",
                Wasm.VI64 2
              ),
              ( asTest "case (6: Int64) { 1 -> 1, _ -> 0 }",
                Wasm.VI64 0
              ),
              (asTest "case ((1:Int64),(2: Int64)) { (a,b) -> a + b }", Wasm.VI64 3),
              ( asTest "case ((1: Int32),(2:Int32)) { (1,2) -> 1, (2,2) -> 2, (_,_) -> 0 }",
                Wasm.VI64 1
              ),
              ( asTest "case ((1: Int64),(2:Int64)) { (a,2) -> a, (_,_) -> 400 }",
                Wasm.VI64 1
              ),
              (asTest "case Box((42:Int64)) { Box(2) -> 0, Box(a) -> a }", Wasm.VI64 42),
              (asTest "case Box(Box((42:Int64))) { Box(Box(2)) -> 0, Box(Box(a)) -> a }", Wasm.VI64 42),
              ( asTest $
                  joinLines
                    [ "if True then ",
                      "{ let box: Box(Int64) = Box(100); let Box(b) = box; 1 + b}",
                      "else 400"
                    ],
                Wasm.VI64 101
              ),
              ( asTest $
                  joinLines
                    [ "let struct: (Box(Int64), Box(Int64)) = (Box(1), Box(2));",
                      "case struct { (Box(a), Box(2)) -> a, (_,_) -> 400 }"
                    ],
                Wasm.VI64 1
              ),
              ( asTest $
                  joinLines
                    [ "let box = Box((100: Int64)); let Box(b) = box; 1 + b"
                    ],
                Wasm.VI64 101
              ),
              ( asTest $
                  joinLines
                    [ "case (1:Int64) { 1 -> { let Box(b) = Box((100: Int64)); 1 + b}, _ -> 400 }"
                    ],
                Wasm.VI64 101
              ),
              ( asTest $
                  joinLines
                    [ "let pair = ((1:Int64),(2:Int64));",
                      "case pair { ",
                      "(1,2) -> 202,",
                      "(a,_) -> { let box = Box((100: Int64)); let Box(b) = box; a + b}",
                      "}"
                    ],
                Wasm.VI64 202
              ),
              ( joinLines
                  [ "type Maybe<a> = Just(a) | Nothing",
                    asTest "case (Nothing:Maybe(Int64)) { Just(a) -> a + 1, Nothing -> 0 }"
                  ],
                Wasm.VI64 0 -- quite disappointing we can't infer this from use, really we need to go all-in on HM to make all of this a bit friendlier
              ),
              ( joinLines
                  [ "type Maybe<a> = Just(a) | Nothing",
                    asTest "case Just((100: Int64)) { Just(a) -> a + 1, Nothing -> 0 }"
                  ],
                Wasm.VI64 101
              ),
              ( joinLines
                  [ "type Maybe<a> = Just(a) | Nothing",
                    asTest "case Just(Box((100: Int64))) { Just(Box(a)) -> a + 1, _ -> 0 }"
                  ],
                Wasm.VI64 101
              ),
              ( joinLines
                  [ "type Maybe<a> = Just(a) | Nothing",
                    asTest "case Just(Just(Box((100: Int64)))) { Just(Just(Box(a))) -> a + 1, _ -> 0 }"
                  ],
                Wasm.VI64 101
              ),
              ( joinLines
                  [ "type Colour = Red | Green | Blue",
                    asTest "case Blue { Red -> 1, Green -> 2, Blue -> 3 }"
                  ],
                Wasm.VI64 3
              ),
              ( joinLines
                  [ "type These<a,b> = This(a) | That(b) | These(a,b)",
                    "export function test() -> Boolean { case These(True,False) { This(a) -> a , That(b) -> b , These(a,b) -> a && b } }"
                  ],
                Wasm.VI32 0
              ),
              ( joinLines
                  [ "type Maybe<a> = Just(a) | Nothing",
                    "function fromMaybe<a>(maybe: Maybe(a), default: a) -> a { case maybe { Just(a) -> a, Nothing -> default } }",
                    asTest "let matchValue: Maybe(Box(Int64)) = Just(Box(100)); let default: Box(Int64) = Box(0); let Box(result) = fromMaybe(matchValue, default); result"
                  ],
                Wasm.VI64 100
              ),
              ( joinLines
                  [ "type List<a> = Cons(a, List(a)) | Nil",
                    asTest "let value: List(Int64) = Cons((1:Int64),Cons((2:Int64),Nil)); case value { Cons(a,Cons(b,Nil)) -> a + b, _ -> 0 }"
                  ],
                Wasm.VI64 3
              ),
              ( joinLines
                  [ "type List<a> = Cons(a, List(a)) | Nil",
                    "function sum(list:List(Int64)) -> Int64 { case list { Cons(a, rest) -> a + sum(rest), Nil -> 0 } }",
                    asTest "sum(Cons(1,Cons(2,Cons(3,Cons(4,Nil)))))"
                  ],
                Wasm.VI64 10
              ),
              ( joinLines
                  [ "type List<a> = Cons(a, List(a)) | Nil",
                    "function repeat(value: Int64, repeats: Int64 ) -> List(Int64) { if repeats < 1 then Nil else Cons(value, repeat(value, repeats - 1)) }",
                    "function sum(accum: Int64,list:List(Int64)) -> Int64 { case list { Cons(a, rest) -> sum(accum + a, rest), Nil -> accum } }",
                    asTest "sum(0,repeat(6,100))" -- surprisingly easy to pop the stack by increasing this value
                  ],
                Wasm.VI64 600
              ),
              ( joinLines
                  [ "type Identity<a> = Identity(a)",
                    asTest "let boxed :Identity(Int64) = Identity(100); case boxed { Identity(a) -> a }"
                  ],
                Wasm.VI64 100
              ),
              ( asTest "let requested: Int64 = 3; let diff = requested % 4; if diff == 0 then requested else requested + (4 - diff)",
              Wasm.VI64 4)
              ,
              ( asTest $
                  joinLines
                    [ "let pair = ((1:Int64),False);",
                      "case pair { ",
                      "(a,False) -> { let allocated = Box((100: Int64)); let Box(b) = allocated; b + a },",
                      "_ -> 400 ",
                      "}"
                    ],
                Wasm.VI64 101
              )
            ]

      describe "From expressions" $ do
        traverse_ testWithInterpreter testVals

      describe "Deallocations for expressions" $ do
        traverse_ testDeallocation testVals

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

-- these are saved in a file that is included in compilation
testJSSource :: LB.ByteString
testJSSource =
  LB.fromStrict $(makeRelativeToProject "test/js/test.mjs" >>= embedFile)

-- | compile module or spit out error
compile :: T.Text -> Wasm.Module
compile input =
  case parseModuleAndFormatError input of
    Left e -> error (show e)
    Right parsedModuleItems ->
      case resolveModule parsedModuleItems of
        Left resolveError -> error (show resolveError)
        Right parsedMod -> case treeShakeModule <$> elaborateModule parsedMod of
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
            ToWasm.wfExpr = expr,
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

-- | test everything is deallocated using the built-in `wasm` package interpreter
testDeallocation :: (T.Text, Wasm.Value) -> Spec
testDeallocation (input, _) = it (show input) $ do
  let actualWasmModule = compile input
  -- do we deallocate everything?
  allocResp <- runWasm "alloccount" actualWasmModule
  allocResp `shouldBe` Just [Wasm.VI32 0]

-- | in fear of getting incredibly meta, run the tests from this module
-- using the built-in `wasm` interpreter
runTestsWithInterpreter :: (T.Text, [(T.Text, Bool)]) -> Spec
runTestsWithInterpreter (input, result) = it (show input) $ do
  case parseModuleAndFormatError input of
    Left e -> error (show e)
    Right parsedModuleItems ->
      case resolveModule parsedModuleItems of
        Left e -> error (show e)
        Right parsedModule -> case elaborateModule parsedModule of
          Left typeErr -> error (show typeErr)
          Right typedMod -> do
            resp <- testModule typedMod
            resp `shouldBe` result

-- | output actual WASM files for testing
-- test them with node
testWithNode :: (T.Text, T.Text) -> Spec
testWithNode (input, result) = it (show input) $ do
  withSystemTempDirectory "wasmnode" $ \directory -> do
    let actualWasmModule = compile input
        inputHash = hash input
        wasmFilename = directory <> "/" <> show inputHash <> ".wasm"
        jsFilename = directory <> "/test.mjs"

    -- write test.js to a file
    liftIO (LB.writeFile jsFilename testJSSource)

    -- write module to a file so we can run it with `wasmtime` etc
    liftIO (writeModule wasmFilename actualWasmModule)

    -- run node js, get output
    (success, output) <- runScriptFromFile wasmFilename jsFilename
    output `shouldBe` T.unpack result
    -- check it succeeded
    success `shouldBe` True
