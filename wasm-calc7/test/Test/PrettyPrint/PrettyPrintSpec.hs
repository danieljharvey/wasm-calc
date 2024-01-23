{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | test evaluating and running tests for a module
module Test.PrettyPrint.PrettyPrintSpec (spec) where

import qualified Calc.Parser               as Parse
import           Calc.Types.Module
import           Data.Bifunctor            (second)
import           Data.FileEmbed
import           Data.Foldable             (traverse_)
import           Data.Functor
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Prettyprinter             as PP
import qualified Prettyprinter.Render.Text as PP
import           Test.Hspec

renderWithWidth :: Int -> PP.Doc ann -> T.Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

-- read a module, pretty print it, is it the same?
parseModule :: Text -> Module ()
parseModule input =
  case Parse.parseModuleAndFormatError input of
    Left e      -> error (show e)
    Right parts -> parts $> ()

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "PrettyPrint" $ do
      let printModule (filepath, input) =
            it ("Pretty pretting " <> filepath <> " round trips successfully") $ do
              let parts = parseModule input
                  printed = renderWithWidth 60 $ PP.pretty parts
              let parts2 = parseModule printed
              parts2 `shouldBe` parts
      traverse_ printModule testInputs

    describe "PrettyPrint is saved" $ do
      let printModule (filepath, input) =
            it ("Pretty pretting " <> filepath <> " is the same") $ do
              let parts = parseModule input
                  printed = renderWithWidth 60 $ PP.pretty parts
              printed `shouldBe` input
      traverse_ printModule testInputs
