{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE LambdaCase #-}
module Test.Typecheck.ExhaustivenessSpec (spec) where
import Calc.Typecheck

import Calc.Parser
import Control.Monad
import Data.Foldable (traverse_)
import Test.Hspec
import qualified Data.Text as T
import Calc.Typecheck.Exhaustiveness
import qualified Data.Set as S

testGeneratePattern :: (T.Text,T.Text,[T.Text]) -> Spec
testGeneratePattern (patStr, typeStr, expectedStrs) = do
  it ("Generates for " <> show patStr) $ do
    let pat = fromRight $ parsePatternAndFormatError patStr
        ty = fromRight $ parseTypeAndFormatError typeStr
        typedPat = fromRight $ runTC (checkPattern ty pat)
        expected = S.fromList $ fromRight . parsePatternAndFormatError <$> expectedStrs
    generateFromPattern typedPat `shouldBe` S.map void expected

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


spec :: Spec
spec = do
  fdescribe "ExhaustivenessSpec" $ do
    describe "Generate" $ do
      let examples =
            [ ("_", "a", []),
              ("True", "Boolean", ["False"]),
              ("False", "Boolean", ["True"]),
              ("1", "Int64", ["_"])
                ,
                ("1.1", "Float64", ["_"]),
                ("a", "a", []),
                ("(a,b)", "(a,b)", []),
                ("(1,2)","(Int64,Int64)", ["(_,_)"])

            ]

      describe "Successfully generates patterns" $ do
        traverse_ testGeneratePattern examples


