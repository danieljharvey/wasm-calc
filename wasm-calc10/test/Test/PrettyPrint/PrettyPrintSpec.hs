{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | test evaluating and running tests for a module
module Test.PrettyPrint.PrettyPrintSpec (spec) where

import qualified Calc.Parser        as Parse
import           Calc.PrettyPrint
import           Calc.Types.Module
import           Data.Bifunctor     (second)
import           Data.FileEmbed
import           Data.Foldable      (traverse_)
import           Data.Functor
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import           Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

-- read a module, pretty print it, is it the same?
parseModule :: Text -> [ModuleItem ()]
parseModule input =
  case Parse.parseModuleAndFormatError input of
    Left e      -> error (show e)
    Right parts -> void <$> parts

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "PrettyPrint" $ do
      let printModule (filepath, input) =
            it ("Pretty pretting " <> filepath <> " round trips successfully") $ do
              let parts = parseModule input
                  printed = format parts
              let parts2 = parseModule printed
              parts2 `shouldBe` parts
      traverse_ printModule testInputs

    describe "PrettyPrint is saved" $ do
      let printModule (filepath, input) =
            it ("Pretty pretting " <> filepath <> " is the same") $ do
              let parts = parseModule input
                  printed = format parts
              printed `shouldBe` input
      traverse_ printModule testInputs
