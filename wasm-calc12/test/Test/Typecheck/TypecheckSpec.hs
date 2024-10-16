{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Typecheck.TypecheckSpec (spec) where

import Calc.ExprUtils
import Calc.Module
import Calc.Parser
import Calc.Typecheck
import Calc.Types
import Control.Monad
import Data.Bifunctor (second)
import Data.Either (isLeft, isRight)
import Data.FileEmbed
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Test.Helpers
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

spec :: Spec
spec = do
  describe "TypecheckSpec" $ do
    describe "Function" $ do
      let succeeding =
            [ ("function one () -> Int64 { 1 }", TFunction () [] tyInt64),
              ("function int32() -> Int32 { 1 }", TFunction () [] tyInt32),
              ( "function not (bool: Boolean) -> Boolean { if bool then False else True }",
                TFunction () [tyBool] tyBool
              ),
              ( "function swapPair<a,b>(pair: (a,b)) -> (b,a) { let (a,b) = pair; (b,a) }",
                TFunction
                  ()
                  [tyContainer [tyVar "a", tyVar "b"]]
                  (tyContainer [tyVar "b", tyVar "a"])
              ),
              ( "function sumTuple(pair: (Float64, Float64)) -> Float64 { let (a,b) = pair; a + b }",
                TFunction
                  ()
                  [tyContainer [tyFloat64, tyFloat64]]
                  tyFloat64
              ),
              ( "function factorial(a: Int64) -> Int64 { if a == 0 then 1 else a * factorial(a - 1) }",
                TFunction () [tyInt64] tyInt64
              )
            ]

      describe "Successfully typechecking functions" $ do
        traverse_ testSucceedingFunction succeeding

    describe "Module" $ do
      let succeeding =
            [ ( joinLines
                  [ "function ignore() -> Int64 { 1 }",
                    "function main() -> Int64 { 42 }"
                  ],
                tyInt64
              ),
              ( joinLines
                  [ "function increment(a: Int64) -> Int64 { a + 1 }",
                    "function main() -> Int64 { increment(41) }"
                  ],
                tyInt64
              ),
              ( joinLines
                  [ "function inc(a: Int64) -> Int64 { a + 1 }",
                    "function inc2(a: Int64) -> Int64 { inc(a) }",
                    "function main() -> Int64 {inc2(41) }"
                  ],
                tyInt64
              ),
              ( joinLines
                  [ "function swapPair<a,b>(pair: (a,b)) -> (b,a) { let (a,b) = pair; (b,a) }",
                    "function main() -> (Int64, Boolean) { swapPair((True,1)) }"
                  ],
                tyContainer [tyInt64, tyBool]
              ),
              ( joinLines
                  [ "function boxedId<a>(value: a) -> a { value }",
                    "function main() -> Box(Int64) {boxedId(Box(1)) }"
                  ],
                tyContainer [tyInt64]
              ),
              ( joinLines
                  [ "function unboxedReturnFst<a,b>(pair: (a,b)) -> a { let (a, _) = pair; a }",
                    "function main() -> Box(Int64) { unboxedReturnFst((Box(1),Box((2 : Int32)))) }"
                  ],
                tyContainer [tyInt64]
              ),
              ( joinLines
                  [ "function main() -> Box(Int32) { Box(1) }"
                  ],
                tyContainer [tyInt32]
              ),
              ( joinLines
                  [ "import maths.add as add(a: Int64, b: Int64) -> Int64",
                    "function main() -> Int64 { add(1,2) }"
                  ],
                tyInt64
              ),
              ( joinLines
                  [ "function acceptInt32(a: Int32) -> Int32 { a }",
                    "function main() -> Int32 { acceptInt32(1) }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "memory 1000",
                    "function main() -> Int32 { load(0) }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "memory 1000",
                    "function main() -> Int32 { store(0, (100: Int32)); load(0) }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "global one: Int32 = 1",
                    "function main() -> Int64 { 1 }"
                  ],
                tyInt64
              ),
              ( joinLines
                  [ "global one: Int32 = 1",
                    "function main() -> Int32 { one }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "global one: Int32 = 1",
                    "global two = one + one",
                    "function main() -> Int32 { two }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "global mut counter: Int32 = 0",
                    "function increment() -> Void { let current = counter; set(counter, current + 1) }",
                    "function main() -> Int32 { increment(); increment(); counter }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "function main() -> Int32 { 1 }",
                    "test itsTrue = True"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "type Colour = Red | Green",
                    "function main() -> Colour { let a = Red; a }"
                  ],
                tyConstructor "Colour" []
              ),
              ( joinLines
                  [ "type Maybe<a> = Just(a) | Nothing",
                    "function main() -> Maybe(Int32) { Just(100) }"
                  ],
                tyConstructor "Maybe" [tyInt32]
              ),
              ( joinLines
                  [ "type Maybe<a> = Just(a) | Nothing",
                    "function main() -> Maybe(Int32) { Nothing }"
                  ],
                tyConstructor "Maybe" [tyInt32]
              ),
              ( joinLines
                  [ "type Either<e,a> = Left(e) | Right(a)",
                    "function main() -> Either(Boolean,Int32) { Right(100) }"
                  ],
                tyConstructor "Either" [tyBool, tyInt32]
              ),
              ( joinLines
                  [ "type These<a,b> = This(a) | That(b) | These(a,b)",
                    "function main() -> These(Boolean,Int32) { These(True,100) }"
                  ],
                tyConstructor "These" [tyBool, tyInt32]
              ),
              ( joinLines
                  [ "type Either<e,a> = Left(e) | Right(a)",
                    "function main() -> Int32 { let _ = Right(True); 300 }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "type List<a> = Cons(a, List(a)) | Nil",
                    "function main() -> Int32 { let _: List(Boolean) = Cons(True, Cons(False, Nil)); 100 }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "type List<a> = Cons(a, List(a)) | Nil",
                    "function main() -> Int32 { let _ = Cons(True, Cons(False, Nil)); 100 }"
                  ],
                tyInt32
              ),
              ( "function main() -> Int32 { case True { True -> 1, False -> 2 } }",
                tyInt32
              ),
              ( joinLines
                  [ "type Either<e,a> = Left(e) | Right(a)",
                    "function main() -> Int32 { case Right((42:Int32)) { Right(a) -> a, Left(_) -> 0 } }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "type Either<e,a> = Left(e) | Right(a)",
                    "function main() -> Int32 { case Right((42:Int32)) { Right(a) -> a, Left(e) -> e } }"
                  ],
                tyInt32
              ),
              ( joinLines
                  [ "type Either<e,a> = Left(e) | Right(a)",
                    "function main() -> Int32 { let either: Either(Boolean,Int32) = Right(42); case either { Right(a) -> a, Left(_) -> 0 } }"
                  ],
                tyInt32
              )
            ]
      describe "Successfully typechecking modules" $ do
        traverse_ testSucceedingModule succeeding

      describe "Successfully typechecking modules" $ do
        traverse_ (uncurry testModuleTypechecks) testInputs

      let failing =
            [ joinLines
                [ "function increment(b: Boolean) -> Boolean { a + 1 }",
                  "function main() -> Int64 { increment(41) }"
                ],
              joinLines
                [ "function usesNonBoxedGeneric<a>(ohno: a) -> a { ohno }",
                  "function main() -> Int32 { usesNonBoxedGeneric(1) }"
                ],
              joinLines
                [ "function unboxedReturnFst<a,b>(pair: (a,b)) -> a { let (a, _) = pair; a }",
                  "function main() -> Int32 { unboxedReturnFst((1,(2 : Int32))) }"
                ],
              joinLines
                [ "function fst<a,b>(pair: (a,b)) -> Box(a) { let (a, _) = pair; Box(a) }",
                  "function main() -> Int64 { let Box(a) = fst(((1: Int64), (2: Int64))); a }"
                ],
              joinLines
                [ "import console.log as log(a: Int64) -> Void",
                  "function main() -> Int32 { let a = log(1); a }"
                ],
              "global one = 1",
              joinLines
                [ "global mut counter: Int32 = 0",
                  "function setsWithWrongType() -> Void { set(counter, True) }"
                ],
              joinLines
                [ "global counter: Int32 = 0",
                  "function setsNonMutableGlobal() -> Void { set(counter, 1) }"
                ],
              joinLines ["test itsNotABool = (1: Int32)"],
              joinLines
                [ "type Either<e,a> = Left(e) | Right(a)",
                  "function main() -> Int32 { case Right((42:Int32)) { Right(int) -> int, Left(bool) -> bool && True } }"
                ],
              joinLines
                [ "type List<a> = Cons(a, List(a)) | Nil",
                  "function main() -> Int32 { let _ = Cons(True, Cons((42:Int32),Nil)); 100 }"
                ]
            ]
      describe "Failing typechecking modules" $ do
        traverse_ testFailingModule failing

    describe "Expr" $ do
      let succeeding =
            [ ("(42 : Int8)", "Int8"),
              ("True", "Boolean"),
              ("1 + (1 : Int64)", "Int64"),
              ("6 * (9 : Int64)", "Int64"),
              ("(1 - 10 : Int64)", "Int64"),
              ("2 == (2 : Int64)", "Boolean"),
              ("2 > (2: Int64)", "Boolean"),
              ("(2: Int64) >= 2", "Boolean"),
              ("2 < (2: Int64)", "Boolean"),
              ("(2: Int64) <= 2", "Boolean"),
              ("(2: Int64) % 2", "Int64"),
              ("1.0 + (2.0: Float32)", "Float32"),
              ("(10.0 : Float64) * 10.0", "Float64"),
              ("if True then (1: Int64) else 2", "Int64"),
              ("if False then True else False", "Boolean"),
              ("((1: Int64), (2: Int64), True)", "(Int64,Int64,Boolean)"),
              ("Box((1: Int64))", "Box(Int64)"),
              ("let Box(a) = Box((1: Int64)); a", "Int64"),
              ("let a: Int64 = 100; a", "Int64"),
              ("let (a,b): (Int64,Int64) = (1,2); a + b", "Int64"),
              ("True && True", "Boolean"),
              ("False || True", "Boolean"),
              ( "let inner = Box((100: Int64)); let Box(inner2) = Box(inner); let Box(inner3) = inner2; inner3",
                "Int64"
              ),
              ( "let Box(outer) = Box(Box((100: Int64))); let Box(inner) = outer; inner",
                "Int64"
              ),
              ("case True { True -> (1: Int64), False -> (2: Int64) }", "Int64"),
              ("\\() -> Int32 { 3 }", "Fn() -> Int32"),
              ( "\\(a: Int32,b:Int32) -> Int32 { a + b }",
                "Fn(Int32,Int32) -> Int32"
              ),
              ("let f = \\(a: Int32) -> Int32 { a }; f(100)", "Int32")
            ]

      describe "Successfully typechecking expressions" $ do
        traverse_ testSucceedingExpr succeeding

      let failing =
            [ ("if (1: Int64) then 1 else 2", PredicateIsNotBoolean () tyInt64),
              ("if True then (1: Int64) else True", TypeMismatch tyBool tyInt64),
              ("(1: Int64) + (1.0: Float64)", InfixTypeMismatch OpAdd tyInt64 tyFloat64),
              ("(1: Int64) + True", InfixTypeMismatch OpAdd tyInt64 tyBool),
              ("True + False", InfixTypeMismatch OpAdd tyBool tyBool),
              ("(1 : Int64) * False", InfixTypeMismatch OpMultiply tyInt64 tyBool),
              ("True - (1 : Int64)", InfixTypeMismatch OpSubtract tyBool tyInt64),
              ("True && (1 : Int64)", TypeMismatch tyBool tyInt64),
              ("True || (1 : Int64)", TypeMismatch tyBool tyInt64),
              ( "let (a,b) = (1 : Int64); a + b",
                PatternMismatch
                  tyInt64
                  ( PTuple
                      ()
                      (PVar () "a")
                      (NE.singleton $ PVar () "b")
                  )
              ),
              ( "let (a,b,c) = ((1 : Int64), (2 : Int64)); a + b",
                PatternMismatch
                  (tyContainer [tyInt64, tyInt64])
                  ( PTuple
                      ()
                      (PVar () "a")
                      (NE.fromList [PVar () "b", PVar () "c"])
                  )
              ),
              ("case True { True -> (1: Int64), 1 -> (2: Int64) }", PatternMismatch tyBool (PLiteral () (PIntLit 1))),
              ("case True { True -> (1: Int64), False -> False }", TypeMismatch tyInt64 tyBool),
              ("case True { True -> True , True -> False }", PatternMatchError (MissingPatterns () [PLiteral () (PBool False)])),
              ( "let (_,False) = (True,False); True",
                PatternMatchError
                  ( MissingPatterns
                      ()
                      [ PTuple () (PWildcard ()) (NE.singleton $ PLiteral () (PBool True))
                      ]
                  )
              )
            ]

      describe "Failing typechecking expressions" $ do
        traverse_ testFailingExpr failing

runTC :: TypecheckM ann a -> Either (TypeError ann) a
runTC =
  runTypecheckM
    ( TypecheckEnv
        { tceVars = mempty,
          tceGenerics = mempty,
          tceMemoryLimit = 0,
          tceDataTypes = mempty
        }
    )

testSucceedingExpr :: (Text, Text) -> Spec
testSucceedingExpr (input, result) = it (show input) $ do
  case (,) <$> parseExprAndFormatError input <*> parseTypeAndFormatError result of
    Left e -> error (show e)
    Right (expr, tyResult) -> do
      getOuterAnnotation <$> runTC (infer (void expr))
        `shouldBe` Right (void tyResult)

testFailingExpr :: (Text, TypeError ()) -> Spec
testFailingExpr (input, result) = it (show input) $ do
  case parseExprAndFormatError input of
    Left e -> error (show e)
    Right expr -> do
      getOuterAnnotation <$> runTC (infer (void expr))
        `shouldBe` Left result

testSucceedingFunction :: (Text, Type ()) -> Spec
testSucceedingFunction (input, fn) =
  it (show input) $ do
    case parseFunctionAndFormatError input of
      Left e -> error (show e)
      Right parsedFn ->
        fnAnn <$> runTC (elaborateFunction (void parsedFn))
          `shouldBe` Right fn

testSucceedingModule :: (Text, Type ()) -> Spec
testSucceedingModule (input, md) =
  it (show input) $ do
    case parseModuleAndFormatError input of
      Left e -> error (show e)
      Right parsedModuleItems ->
        case resolveModule parsedModuleItems of
          Left e -> error (show e)
          Right parsedMod ->
            getOuterAnnotation . fnBody . getMainFunction <$> elaborateModule (void parsedMod)
              `shouldBe` Right md

testModuleTypechecks :: String -> Text -> Spec
testModuleTypechecks fileName input =
  it fileName $ do
    case parseModuleAndFormatError input of
      Left e -> error (show e)
      Right parsedModuleItems ->
        case resolveModule parsedModuleItems of
          Left e -> error (show e)
          Right parsedMod -> do
            let result = elaborateModule (void parsedMod)
            case result of
              Right _ -> pure ()
              Left e -> error (show e)
            isRight result `shouldBe` True

-- | find function called 'main'
getMainFunction :: Module ann -> Function ann
getMainFunction (Module {mdFunctions}) =
  case List.find (\fn -> fnFunctionName fn == "main") mdFunctions of
    Just fn -> fn
    Nothing -> error "Could not find 'main' function"

testFailingModule :: Text -> Spec
testFailingModule input =
  it (show input) $ do
    case parseModuleAndFormatError input of
      Left e -> error (show e)
      Right parsedModuleItems ->
        case resolveModule parsedModuleItems of
          Left e -> error (show e)
          Right parsedMod ->
            elaborateModule (void parsedMod)
              `shouldSatisfy` isLeft
