{-# LANGUAGE OverloadedStrings #-}

module Test.Linearity.LinearitySpec (spec) where

import Calc
import Calc.Linearity
import Calc.Typecheck
import Control.Monad (void)
import Data.Either (isRight)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

runTC :: TypecheckM ann a -> Either (TypeError ann) a
runTC = runTypecheckM (TypecheckEnv mempty mempty)

spec :: Spec
spec = do
  describe "LinearitySpec" $ do
    describe "getFunctionUses" $ do
      let strings =
            [ ( "function sum (a: Integer, b: Integer) { a + b }",
                LinearState
                  { lsVars =
                      M.fromList [("a", (LTPrimitive, ())), ("b", (LTPrimitive, ()))],
                    lsUses = [("b", Whole), ("a", Whole)]
                  }
              ),
              ( "function pair<a,b>(a: a, b: b) { (a,b) }",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ())), ("b", (LTBoxed, ()))],
                    lsUses = [("b", Whole), ("a", Whole)]
                  }
              ),
              ( "function dontUseA<a,b>(a: a, b: b) { b }",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ())), ("b", (LTBoxed, ()))],
                    lsUses = [("b", Whole)]
                  }
              ),
              ( "function dup<a>(a: a) { (a,a)}",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ()))],
                    lsUses = [("a", Whole), ("a", Whole)]
                  }
              ),
              ( "function main() { let a = 1; let b = Box(a); b! }",
                LinearState
                  { lsVars =
                      M.fromList
                        [ ("a", (LTPrimitive, ())),
                          ("b", (LTBoxed, ()))
                        ],
                    lsUses = [("b", Slice 1), ("a", Whole)]
                  }
              )
            ]
      traverse_
        ( \(str, linearState) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> do
                case runTC (elaborateFunction parsedFn) of
                  Left e -> error (show e)
                  Right typedFn ->
                    void (getFunctionUses typedFn) `shouldBe` linearState
              Left e -> error (T.unpack e)
        )
        strings

    describe "validateFunction" $ do
      describe "expected successes" $ do
        let success =
              [ "function sum (a: Integer, b: Integer) { a + b }",
                "function pair<a,b>(a: a, b: b) { (a,b) }",
                "function main() { let a = 1; let b = Box(a); b! }",
                "function addPair(pair: (Integer,Integer)) { let (a,b) = pair; a + b }",
                "function fst<a,b>(pair: (a,b)) { let (a,_) = pair; Box(a) }",
                "function main() { let _ = 1; 2 }"
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
                  NotUsed () "a"
                ),
                ( "function dontUsePrimA(a: Integer, b: Integer) { b }",
                  NotUsed () "a"
                ),
                ( "function dup<a>(a: a) { (a,a)}",
                  UsedMultipleTimes "a"
                ),
                {-( "function twice(pair: (Integer, Integer)) { pair.1 + pair.2 }",
                  UsedMultipleTimes "pair"
                ),-}
                ( "function withPair<a,b>(pair: (a,b)) { let (a,b) = pair; (a, a, b) }",
                  UsedMultipleTimes "a"
                )
              ]
        traverse_
          ( \(str, err) -> it (T.unpack str) $ do
              case parseFunctionAndFormatError str of
                Right parsedFn -> do
                  case runTC (elaborateFunction (void parsedFn)) of
                    Left e -> error (show e)
                    Right typedFn ->
                      validateFunction typedFn `shouldBe` Left err
                Left e -> error (T.unpack e)
          )
          failures
