{-# LANGUAGE NamedFieldPuns #-}
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
import Calc.Wasm.FromExpr.Helpers (getOffsetList, getOffsetListForConstructor, monomorphiseTypes)
import Calc.Wasm.FromExpr.Patterns.Predicates
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad (void)
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Helpers
import Test.Hspec

unsafeTy :: T.Text -> Type ()
unsafeTy tyString =
  case parseTypeAndFormatError tyString of
    Left e -> error (show e)
    Right ty -> void ty

exprState :: FromExprState
exprState =
  FromExprState
    { fesFunctions = mempty,
      fesImports = mempty,
      fesGlobals = mempty,
      fesVars = mempty,
      fesArgs = mempty,
      fesGenerated = mempty,
      fesDataTypes
    }
  where
    fesDataTypes =
      M.fromList
        [ ( DataName "Maybe",
            [ FromExprConstructor "Just" [Pointer],
              FromExprConstructor "Nothing" []
            ]
          ),
          ( DataName "These",
            [ FromExprConstructor "This" [Pointer],
              FromExprConstructor "That" [Pointer],
              FromExprConstructor "These" [Pointer, Pointer]
            ]
          )
        ]

spec :: Spec
spec = do
  describe "FromWasmSpec" $ do
    describe "getOffsetList" $ do
      it "Tuple of ints" $ do
        getOffsetList (unsafeTy "(Int32,Int32,Int64)")
          `shouldBe` [0, 4, 8, 16]

      it "Tuple of smaller ints" $ do
        getOffsetList (unsafeTy "(Int8,Int8,Int64)")
          `shouldBe` [0, 1, 2, 10]

    describe "getOffsetListForConstructor" $ do
      it "Construct with single item" $ do
        flip evalStateT exprState (getOffsetListForConstructor (unsafeTy "Maybe(Int8)") "Just")
          `shouldBe` Right [1, 5]

      it "Construct with two items" $ do
        flip evalStateT exprState (getOffsetListForConstructor (unsafeTy "These(Int8,Int64)") "These")
          `shouldBe` Right [1, 5, 9]

    describe "calculateMonomorphisedTypes" $ do
      it "Ints" $ do
        monomorphiseTypes @() ["a", "b"] [tyVar "a", tyVar "b"] [tyInt32, tyInt64]
          `shouldBe` [("a", tyInt32), ("b", tyInt64)]

      it "Ints reversed" $ do
        monomorphiseTypes @() ["a", "b"] [tyVar "b", tyVar "a"] [tyInt64, tyInt32]
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
              flip
                evalStateT
                exprState
                (createDropFunction 1 (unsafeTy tyString))
                `shouldBe` Right wasmFunc
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
              flip
                evalStateT
                exprState
                (typeToDropPaths (unsafeTy tyString) id)
                `shouldBe` Right paths
        )
        testVals

    describe "Predicates" $ do
      describe "predicateToWasm" $ do
        let testVals =
              [ ( Equals [] tyBool (PBool True),
                  WVar 100,
                  WInfix
                    I32
                    OpEquals
                    (WPrim (WPBool True))
                    (WVar 100)
                ),
                ( Equals [] tyInt32 (PIntLit 42),
                  WVar 100,
                  WInfix
                    I32
                    OpEquals
                    (WPrim (WPInt32 42))
                    (WVar 100)
                ),
                ( Equals [(tyInt32, 0)] tyInt32 (PIntLit 42),
                  WVar 100,
                  WInfix
                    I32
                    OpEquals
                    (WPrim (WPInt32 42))
                    (WTupleAccess I32 (WVar 100) 0)
                ),
                ( Equals [(tyInt32, 0), (tyInt64, 1)] tyInt32 (PIntLit 42),
                  WVar 100,
                  WInfix
                    I32
                    OpEquals
                    (WPrim (WPInt32 42))
                    (WTupleAccess I64 (WTupleAccess I32 (WVar 100) 0) 1)
                )
              ]
        traverse_
          ( \(predicate, val, expected) ->
              it (show predicate) $ do
                flip evalStateT exprState (predicateToWasm @_ @() val predicate)
                  `shouldBe` Right expected
          )
          testVals
