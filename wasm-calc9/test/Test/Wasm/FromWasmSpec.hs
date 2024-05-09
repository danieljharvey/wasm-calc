{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.FromWasmSpec (spec) where

import           Calc.Parser
import           Calc.Types.Type
import           Calc.Wasm.FromExpr.Drops (DropPath (..), typeToDropPaths)
import           Control.Monad            (void)
import           Data.Foldable            (traverse_)
import qualified Data.Text                as T
import           Test.Hspec

unsafeTy :: T.Text -> Type ()
unsafeTy tyString =
  case parseTypeAndFormatError tyString of
    Left e   -> error (show e)
    Right ty -> void ty

spec :: Spec
spec = do
  fdescribe "FromWasmSpec" $ do
    describe "typeToDropPaths" $ do
      let testVals =
            [ ( "Int64",
                []
              ),
              ( "Box(Int64)",
                [DropPathFetch Nothing]
              ),
              ( "(a,b)",
                [ DropPathSelect 0 (DropPathFetch (Just "a")),
                  DropPathSelect 4 (DropPathFetch (Just "b")),
                  DropPathFetch Nothing
                ]
              ),
              ( "(Int64,b)",
                [ DropPathSelect 8 (DropPathFetch (Just "b")),
                  DropPathFetch Nothing
                ]
              )
            ]

      traverse_
        ( \(tyString, paths) -> do
            it (show tyString) $ do
              typeToDropPaths (unsafeTy tyString) id `shouldBe` paths
        )
        testVals
