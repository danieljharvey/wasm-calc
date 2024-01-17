{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.TypecheckSpec (spec) where

import Calc.ExprUtils
import Calc.Parser
import Calc.Typecheck
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Module
import Calc.Types.Pattern
import Calc.Types.Type
import Control.Monad
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Test.Helpers
import Test.Hspec

runTC :: TypecheckM ann a -> Either (TypeError ann) a
runTC = runTypecheckM (TypecheckEnv mempty mempty)

testTypecheck :: (Text, Text) -> Spec
testTypecheck (input, result) = it (show input) $ do
  case (,) <$> parseExprAndFormatError input <*> parseTypeAndFormatError result of
    Left e -> error (show e)
    Right (expr, tyResult) -> do
      getOuterAnnotation <$> runTC (infer (void expr))
        `shouldBe` Right (void tyResult)

testFailing :: (Text, TypeError ()) -> Spec
testFailing (input, result) = it (show input) $ do
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
      Right parsedMod ->
        getOuterAnnotation . fnBody . getMainFunction <$> elaborateModule (void parsedMod)
          `shouldBe` Right md

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
      Right parsedMod ->
        elaborateModule (void parsedMod)
          `shouldSatisfy` isLeft

spec :: Spec
spec = do
  describe "TypecheckSpec" $ do
    describe "Function" $ do
      let succeeding =
            [ ("function one () { 1 }", TFunction () [] tyInt64),
              ( "function not (bool: Boolean) { if bool then False else True }",
                TFunction () [tyBool] tyBool
              ),
              ( "function swapPair<a,b>(pair: (a,b)) { (pair.2, pair.1) }",
                TFunction
                  ()
                  [tyContainer [tyVar "a", tyVar "b"]]
                  (tyContainer [tyVar "b", tyVar "a"])
              )
            ]

      describe "Successfully typechecking functions" $ do
        traverse_ testSucceedingFunction succeeding

    describe "Module" $ do
      let succeeding =
            [ ( joinLines
                  [ "function ignore() { 1 }",
                    "function main() { 42 }"
                  ],
                tyInt64
              ),
              ( joinLines
                  [ "function increment(a: Int64) { a + 1 }",
                    "function main() { increment(41) }"
                  ],
                tyInt64
              ),
              ( joinLines
                  [ "function inc(a: Int64) { a + 1 }",
                    "function inc2(a: Int64) { inc(a) }",
                    "function main() {inc2(41) }"
                  ],
                tyInt64
              ),
              ( joinLines
                  [ "function swapPair<a,b>(pair: (a,b)) { (pair.2, pair.1) }",
                    "function main() { swapPair((True,1)) }"
                  ],
                tyContainer [tyInt64, tyBool]
              ),
              ( joinLines
                  [ "function boxedId<a>(value: a) { value }",
                    "function main() {boxedId(Box(1)) }"
                  ],
                tyContainer [tyInt64]
              ),
              ( joinLines
                  [ "function unboxedReturnFst<a,b>(pair: (a,b)) { pair.1 }",
                    "function main() { unboxedReturnFst((Box(1),Box(2))) }"
                  ],
                tyContainer [tyInt64]
              ),
              ( joinLines
                  [ "import maths.add as add(a: Int64, b: Int64) -> Int64",
                    "function main() { add(1,2) }"
                  ],
                tyInt64
              )
            ]
      describe "Successfully typechecking modules" $ do
        traverse_ testSucceedingModule succeeding

      let failing =
            [ joinLines
                [ "function increment(b: Boolean) { a + 1 }",
                  "function main() { increment(41) }"
                ],
              joinLines
                [ "function usesNonBoxedGeneric<a>(ohno: a) { ohno }",
                  "function main() { usesNonBoxedGeneric(1) }"
                ],
              joinLines
                [ "function unboxedReturnFst<a,b>(pair: (a,b)) { pair.1 }",
                  "function main() { unboxedReturnFst((1,2)) }"
                ],
              joinLines
                [ "import console.log as log(a: Int64) -> Void",
                  "function main() { let a = log(1); a }"
                ]
            ]
      describe "Failing typechecking modules" $ do
        traverse_ testFailingModule failing

    describe "Expr" $ do
      let succeeding =
            [ ("42", "Int64"),
              ("True", "Boolean"),
              ("1 + 1", "Int64"),
              ("6 * 9", "Int64"),
              ("1 - 10", "Int64"),
              ("2 == 2", "Boolean"),
              ("1.0 + 2.0", "Float64"),
              ("10.0 * 10.0", "Float64"),
              ("if True then 1 else 2", "Int64"),
              ("if False then True else False", "Boolean"),
              ("(1,2,True)", "(Int64,Int64,Boolean)"),
              ("(1,2,3).2", "Int64"),
              ("Box(1)", "Box(Int64)"),
              ("Box(1).1", "Int64"),
              ("let a = 100; a", "Int64"),
              ("let (a,b) = (1,2); a + b", "Int64")
            ]

      describe "Successfully typechecking expressions" $ do
        traverse_ testTypecheck succeeding

      let failing =
            [ ("if 1 then 1 else 2", PredicateIsNotBoolean () tyInt64),
              ("if True then 1 else True", TypeMismatch tyInt64 tyBool),
              ("1 + 1.0", InfixTypeMismatch OpAdd tyInt64 tyFloat64),
              ("1 + True", InfixTypeMismatch OpAdd tyInt64 tyBool),
              ("True + False", InfixTypeMismatch OpAdd tyBool tyBool),
              ("1 * False", InfixTypeMismatch OpMultiply tyInt64 tyBool),
              ("True - 1", InfixTypeMismatch OpSubtract tyBool tyInt64),
              ( "let (a,b) = 1; a + b",
                PatternMismatch
                  tyInt64
                  ( PTuple
                      ()
                      (PVar () "a")
                      (NE.singleton $ PVar () "b")
                  )
              ),
              ( "let (a,b,c) = (1,2); a + b",
                PatternMismatch
                  (tyContainer [tyInt64, tyInt64])
                  ( PTuple
                      ()
                      (PVar () "a")
                      (NE.fromList [PVar () "b", PVar () "c"])
                  )
              )
            ]

      describe "Failing typechecking expressions" $ do
        traverse_ testFailing failing
