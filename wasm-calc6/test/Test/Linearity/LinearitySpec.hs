{-# LANGUAGE OverloadedStrings #-}

module Test.Linearity.LinearitySpec (spec) where

import           Calc
import           Calc.Linearity
import           Data.Foldable   (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           Test.Hspec

spec :: Spec
spec = do
  describe "LinearitySpec" $ do
    describe "Function" $ do
      let strings =
            [
              ( "function sum (a: Integer, b: Integer) { a + b }",
                M.fromList [("a",Primitive),("b",Primitive)]
              ),
              ( "function pair<a,b>(a: a, b: b) { (a,b) }",
                M.fromList [("a",Boxed 1),("b",Boxed 1)]

              ),
               ( "function dontUseA<a,b>(a: a, b: b) { b }",
                M.fromList [("a",Boxed 0),("b",Boxed 1)]
              ),
               ( "function dup<a>(a: a) { (a,a)}",
                M.fromList [("a",Boxed 2)]
              )
            ]
      traverse_
        ( \(str, uses) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> getUses parsedFn `shouldBe` uses
              Left e         -> error (T.unpack e)
        )
        strings
