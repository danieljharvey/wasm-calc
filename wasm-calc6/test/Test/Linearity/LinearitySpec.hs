{-# LANGUAGE OverloadedStrings #-}

module Test.Linearity.LinearitySpec (spec) where

import Calc
import Calc.Linearity
import Calc.Typecheck.Elaborate
import Calc.Typecheck.Error
import Calc.Typecheck.Helpers
import Calc.Typecheck.Types
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

runTC :: TypecheckM ann a -> Either (TypeError ann) a
runTC = runTypecheckM (TypecheckEnv mempty mempty)

spec :: Spec
spec = do
  describe "LinearitySpec" $ do
    describe "Function" $ do
      let strings =
            [ ( "function sum (a: Integer, b: Integer) { a + b }",
                M.fromList [("a", Primitive), ("b", Primitive)]
              ),
              ( "function pair<a,b>(a: a, b: b) { (a,b) }",
                M.fromList [("a", Boxed 1), ("b", Boxed 1)]
              ),
              ( "function dontUseA<a,b>(a: a, b: b) { b }",
                M.fromList [("a", Boxed 0), ("b", Boxed 1)]
              ),
              ( "function dup<a>(a: a) { (a,a)}",
                M.fromList [("a", Boxed 2)]
              ),
              ( "function main() { let a = 1; let b = Box(a); b! }",
                M.fromList [("a", Primitive), ("b", Boxed 1)]
              )
            ]
      traverse_
        ( \(str, uses) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> do
                case runTC (elaborateFunction parsedFn) of
                  Left e -> error (show e)
                  Right typedFn ->
                    getUses typedFn `shouldBe` uses
              Left e -> error (T.unpack e)
        )
        strings
