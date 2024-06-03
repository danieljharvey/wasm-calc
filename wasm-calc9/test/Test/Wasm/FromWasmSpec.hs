{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Wasm.FromWasmSpec (spec) where

import Calc.Parser
import Calc.Types
import Calc.Wasm.FromExpr.Drops
  ( DropPath (..),
    createDropFunction,
    typeToDropPaths,
  )
import Calc.Wasm.FromExpr.Helpers (calculateMonomorphisedTypes)
import Calc.Wasm.ToWasm.Types
import Control.Monad (void)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Test.Helpers
import Test.Hspec

unsafeTy :: T.Text -> Type ()
unsafeTy tyString =
  case parseTypeAndFormatError tyString of
    Left e -> error (show e)
    Right ty -> void ty

spec :: Spec
spec = do
  describe "FromWasmSpec" $ do
    describe "calculateMonomorphisedTypes" $ do
      it "Ints" $ do
        calculateMonomorphisedTypes @() ["a", "b"] [tyInt32, tyInt64] [tyVar "a", tyVar "b"]
          `shouldBe` [("a", tyInt32), ("b", tyInt64)]

      it "Ints reversed" $ do
        calculateMonomorphisedTypes @() ["a", "b"] [tyInt64, tyInt32] [tyVar "b", tyVar "a"]
          `shouldBe` [("a", tyInt32), ("b", tyInt64)]

    describe "createDropFunction" $ do
      let wasmFunction =
            WasmFunction
              { wfName = FunctionName "thing",
                wfExpr = WDrop (WVar 0),
                wfPublic = False,
                wfArgs = [],
                wfReturnType = Void,
                wfLocals = [],
                wfAbilities = mempty
              }

      let testVals =
            [ ( "Int64",
                wasmFunction
                  { wfName = FunctionName "drop_1",
                    wfExpr = WReturnVoid,
                    wfArgs = [I64]
                  }
              ),
              ( "Box(Int64)",
                wasmFunction
                  { wfName = FunctionName "drop_1",
                    wfExpr = WDrop (WVar 0),
                    wfArgs = [Pointer]
                  }
              ),
              ( "(Box(Int64),Box(Int64))",
                wasmFunction
                  { wfName = FunctionName "drop_1",
                    wfExpr =
                      WSequence
                        Void
                        ( WSequence
                            Void
                            (WDrop (WTupleAccess Pointer (WVar 0) 0))
                            (WDrop (WTupleAccess Pointer (WVar 0) 4))
                        )
                        (WDrop (WVar 0)),
                    wfArgs = [Pointer]
                  }
              ),
              ( "(a,b)",
                wasmFunction
                  { wfName = FunctionName "drop_1",
                    wfExpr =
                      WSequence
                        Void
                        ( WSequence
                            Void
                            (WApply (WasmGeneratedRef 0) [WTupleAccess Pointer (WVar 0) 0])
                            (WApply (WasmGeneratedRef 1) [WTupleAccess Pointer (WVar 0) 4])
                        )
                        (WDrop (WVar 0)),
                    wfArgs = [Pointer, Pointer, Pointer]
                  }
              )
            ]
      traverse_
        ( \(tyString, wasmFunc) -> do
            it (show tyString) $ do
              createDropFunction 1 (unsafeTy tyString) `shouldBe` Right wasmFunc
        )
        testVals

    describe "typeToDropPaths" $ do
      let testVals =
            [ ( "Int64",
                []
              ),
              ( "Box(Int64)",
                [DropPathFetch Nothing]
              ),
              ( "(a,b)",
                [ DropPathSelect (unsafeTy "a") 0 (DropPathFetch (Just "a")),
                  DropPathSelect (unsafeTy "b") 4 (DropPathFetch (Just "b")),
                  DropPathFetch Nothing
                ]
              ),
              ( "(Int64,b)",
                [ DropPathSelect (unsafeTy "b") 8 (DropPathFetch (Just "b")),
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
