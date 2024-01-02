{-# LANGUAGE OverloadedStrings #-}

module Test.Linearity.LinearitySpec (spec) where

import           Calc
import           Calc.Linearity
import           Calc.Typecheck.Elaborate
import           Calc.Typecheck.Error
import           Calc.Typecheck.Helpers
import           Calc.Typecheck.Types
import           Data.Either              (isRight)
import           Data.Foldable            (traverse_)
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T
import           Test.Hspec

runTC :: TypecheckM ann a -> Either (TypeError ann) a
runTC = runTypecheckM (TypecheckEnv mempty mempty)

spec :: Spec
spec = do
  describe "LinearitySpec" $ do
    describe "getFunctionUses" $ do
      let strings =
            [ ( "function sum (a: Integer, b: Integer) { a + b }",
                M.fromList [(Entire "a", Primitive), (Entire "b", Primitive)]
              ),
              ( "function pair<a,b>(a: a, b: b) { (a,b) }",
                M.fromList [(Entire "a", Boxed 1), (Entire "b", Boxed 1)]
              ),
              ( "function dontUseA<a,b>(a: a, b: b) { b }",
                M.fromList [(Entire "a", Boxed 0), (Entire "b", Boxed 1)]
              ),
              ( "function dup<a>(a: a) { (a,a)}",
                M.fromList [(Entire "a", Boxed 2)]
              ),
              ( "function main() { let a = 1; let b = Box(a); b! }",
                M.fromList [(Entire "a", Primitive), (Entire "b", Boxed 0),
                      (Partial 1 "b", Boxed 1)]
              )
            ]
      traverse_
        ( \(str, uses) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> do
                case runTC (elaborateFunction parsedFn) of
                  Left e -> error (show e)
                  Right typedFn ->
                    getFunctionUses typedFn `shouldBe` uses
              Left e -> error (T.unpack e)
        )
        strings

    describe "validateFunction" $ do
      describe "expected successes" $ do
        let success =
              [ "function sum (a: Integer, b: Integer) { a + b }",
                "function pair<a,b>(a: a, b: b) { (a,b) }",
                "function main() { let a = 1; let b = Box(a); b! }",
                "function addPair(pair: (Integer,Integer)) { pair.1 + pair.2 }"
              ]
        traverse_
          ( \str -> it (T.unpack str) $ do
              case parseFunctionAndFormatError str of
                Right parsedFn -> do
                  case runTC (elaborateFunction parsedFn) of
                    Left e -> error (show e)
                    Right typedFn ->
                      validateFunction typedFn `shouldSatisfy` isRight
                Left e -> error (T.unpack e)
          )
          success
      describe "expected failures" $ do
        let failures =
              [ ( "function dontUseA<a,b>(a: a, b: b) { b }",
                  NotUsed "a"
                ),
                ( "function dup<a>(a: a) { (a,a)}",
                  UsedMultipleTimes "a"
                ),
              ( "function withPair<a,b>(pair: (a,b)) { (pair.1, pair.1, pair.2) }",
                  SliceUsedMultipleTimes "pair" 1
                )

              ]
        traverse_
          ( \(str, err) -> it (T.unpack str) $ do
              case parseFunctionAndFormatError str of
                Right parsedFn -> do
                  case runTC (elaborateFunction parsedFn) of
                    Left e -> error (show e)
                    Right typedFn ->
                      validateFunction typedFn `shouldBe` Left err
                Left e -> error (T.unpack e)
          )
          failures
