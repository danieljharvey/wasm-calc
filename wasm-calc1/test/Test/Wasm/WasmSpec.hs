{-# LANGUAGE OverloadedStrings #-}

module Test.Wasm.WasmSpec (spec) where

import Calc.Parser
import Calc.Wasm.FromExpr
import Calc.Wasm.Run
import Calc.Wasm.Types
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Language.Wasm.Interpreter as Wasm
import Test.Hspec

testCompileExpr :: (Text, Wasm.Value) -> Spec
testCompileExpr (input, result) = it (show input) $ do
  case parseExprAndFormatError input of
    Left e -> error (show e)
    Right expr -> do
      let mod' =
            Module
              { modFunctions =
                  [ Function
                      { fnName = "main",
                        fnExpr = expr,
                        fnPublic = True,
                        fnArgs = mempty,
                        fnReturnType = I32
                      }
                  ]
              }
      resp <- runWasm (createModule mod')
      resp `shouldBe` Just [result]

spec :: Spec
spec = do
  describe "WasmSpec" $ do
    let testVals =
          [ ("42", Wasm.VI32 42),
            ("(1 + 1)", Wasm.VI32 2),
            ("1 + 2 + 3 + 4 + 5 + 6", Wasm.VI32 21),
            ("6 * 6", Wasm.VI32 36),
            ("100 - 1", Wasm.VI32 99)
          ]

    describe "From expressions" $ do
      traverse_ testCompileExpr testVals
