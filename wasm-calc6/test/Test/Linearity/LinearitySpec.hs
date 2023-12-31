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
                M.fromList [("a",1),("b",1)]
              ),
              ( "function pair<a,b>(a: a, b: b) { (a,b) }",
                M.fromList [("a",1),("b",1)]

              ),
               ( "function dontUseA<a,b>(a: a, b: b) { b }",
                M.fromList [("a",0),("b",1)]
              )

            ]
      traverse_
        ( \(str, uses) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> getUses (fnBody parsedFn) `shouldBe` uses
              Left e         -> error (T.unpack e)
        )
        strings
