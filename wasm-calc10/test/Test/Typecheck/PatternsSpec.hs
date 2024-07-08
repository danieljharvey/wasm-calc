{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.PatternsSpec (spec) where

import Calc.Parser
import Calc.Typecheck
import Calc.Typecheck.Patterns
import Control.Monad
import Data.Foldable (traverse_)
import qualified Data.Set as S
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "ExhaustivenessSpec" $ do
    describe "Generate" $ do
      let examples =
            [ ("_", "a", []),
              ("True", "Boolean", ["False"]),
              ("False", "Boolean", ["True"]),
              ("1", "Int64", ["_"]),
              ("1.1", "Float64", ["_"]),
              ("a", "a", []),
              ("(a,b)", "(a,b)", []),
              ("(1,2)", "(Int64,Int64)", ["(_,_)"]),
              ("(True,False)", "(Boolean,Boolean)", ["(False,False)", "(True,True)", "(False,True)"]),
              ("(a,False)", "(a,Boolean)", ["(a,True)"]),
              ( "(1,True,False)",
                "(Int32, Boolean,Boolean)",
                [ "(_, True, True)",
                  "(_,False,False)",
                  "(_,False,True)",
                  "(_,True,False)"
                ]
              )
            ]

      describe "Successfully generates patterns" $ do
        traverse_ testGeneratePattern examples

    describe "missingPatterns" $ do
      let trueExamples =
            [ (["_"], "a", []),
              (["True"], "Boolean", ["False"]),
              (["True", "False"], "Boolean", []),
              (["1", "2", "3"], "Int32", ["_"])
            ]

      describe "Successfully returns missing patterns" $ do
        traverse_ testMissingPatterns trueExamples

    describe "redundantPatterns" $ do
      let trueExamples =
            [ (["_"], "a", []),
              (["True", "False"], "Boolean", []),
              (["_", "1"], "Int32", ["1"]),
              (["1", "_"], "Int32", []),
              (["1", "2", "_"], "Int32", [])
            ]

      describe "Successfully returns redundant patterns" $ do
        traverse_ testRedundantPatterns trueExamples

testGeneratePattern :: (T.Text, T.Text, [T.Text]) -> Spec
testGeneratePattern (patStr, typeStr, expectedStrs) = do
  it ("Generates for " <> show patStr) $ do
    let pat = fromRight $ parsePatternAndFormatError patStr
        ty = fromRight $ parseTypeAndFormatError typeStr
        typedPat = fromRight $ runTC (checkPattern ty pat)
        expected = S.fromList $ fromRight . parsePatternAndFormatError <$> expectedStrs
    generate typedPat `shouldBe` S.map void expected

testMissingPatterns :: ([T.Text], T.Text, [T.Text]) -> Spec
testMissingPatterns (patStrs, typeStr, expectedStrs) = do
  it ("missingPatterns for " <> show patStrs) $ do
    let pats = fromRight . parsePatternAndFormatError <$> patStrs
        ty = fromRight $ parseTypeAndFormatError typeStr
        typedPats = fromRight $ runTC (traverse (checkPattern ty) pats)
        expected = fromRight . parsePatternAndFormatError <$> expectedStrs
    missingPatterns typedPats `shouldBe` void <$> expected

testRedundantPatterns :: ([T.Text], T.Text, [T.Text]) -> Spec
testRedundantPatterns (patStrs, typeStr, expectedStrs) = do
  it ("redundantPatterns for " <> show patStrs) $ do
    let pats = fromRight . parsePatternAndFormatError <$> patStrs
        ty = fromRight $ parseTypeAndFormatError typeStr
        typedPats = fromRight $ runTC (traverse (checkPattern ty) pats)
        expected = fromRight . parsePatternAndFormatError <$> expectedStrs
    void <$> redundantPatterns typedPats `shouldBe` void <$> expected

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

fromRight :: (Show e) => Either e a -> a
fromRight = \case
  Right a -> a
  Left e -> error (show e)
