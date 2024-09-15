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
import Calc.Wasm.FromExpr.Helpers (getOffsetList, getOffsetListForConstructor, memorySizeForType, monomorphiseTypes)
import Calc.Wasm.FromExpr.Patterns.Predicates
import Calc.Wasm.ToWasm.Types
import Control.Monad (void)
import Control.Monad.Identity
import Control.Monad.State
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
    describe "getOffsetList" $ do
      it "Tuple of ints" $ do
        getOffsetList (unsafeTy "(Int32,Int32,Int64)")
          `shouldBe` [0, 4, 8, 16]

      it "Tuple of smaller ints" $ do
        getOffsetList (unsafeTy "(Int8,Int8,Int64)")
          `shouldBe` [0, 1, 2, 10]

    describe "memorySizeForType" $ do
      it "Boolean" $ do
        evalStateT (memorySizeForType (unsafeTy "Boolean")) exprState
          `shouldBe` pure @Identity 1

      it "Int8" $ do
        evalStateT (memorySizeForType (unsafeTy "Int8")) exprState
          `shouldBe` pure @Identity 1

      it "Int32" $ do
        evalStateT (memorySizeForType (unsafeTy "Int32")) exprState
          `shouldBe` pure @Identity 4

      it "Int64" $ do
        evalStateT (memorySizeForType (unsafeTy "Int64")) exprState
          `shouldBe` pure @Identity 8

      it "(Int64,Int64,Int8)" $ do
        evalStateT (memorySizeForType (unsafeTy "(Int64,Int64,Int8)")) exprState
          `shouldBe` pure @Identity 17

      it "Maybe" $ do
        evalStateT (memorySizeForType (unsafeTy "Maybe(Int64)")) exprState
          `shouldBe` pure @Identity 9

      it "Either" $ do
        evalStateT (memorySizeForType (unsafeTy "Either(Int64,Int64)")) exprState
          `shouldBe` pure @Identity 9

      it "These" $ do
        evalStateT (memorySizeForType (unsafeTy "These(Int64,Int64)")) exprState
          `shouldBe` pure @Identity 17

      it "Identity" $ do
        evalStateT (memorySizeForType (unsafeTy "Identity(Int64)")) exprState
          `shouldBe` pure @Identity 8

    describe "getOffsetListForConstructor" $ do
      it "Construct with single item" $ do
        evalStateT (getOffsetListForConstructor (unsafeTy "Maybe(Int8)") "Just") exprState
          `shouldBe` Right [1, 2]

      it "Construct with two items" $ do
        evalStateT (getOffsetListForConstructor (unsafeTy "These(Int8,Int64)") "These") exprState
          `shouldBe` Right [1, 2, 10]

      it "No discriminator when there's only one constructor" $ do
        evalStateT (getOffsetListForConstructor (unsafeTy "Identity(Int8)") "Identity") exprState
          `shouldBe` Right [0, 1]

      it "Construct with two items" $ do
        evalStateT (getOffsetListForConstructor (unsafeTy "These(Int8, Int64)") "This") exprState
          `shouldBe` Right [1, 2]

      it "Recursive type" $ do
        evalStateT (getOffsetListForConstructor (unsafeTy "List(Int64)") "Cons") exprState
          `shouldBe` Right [1, 9, 13]

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
            it (show tyString) $
              evalStateT
                (createDropFunction 1 (unsafeTy tyString))
                exprState
                `shouldBe` Right wasmFunc
        )
        testVals

    describe "typeToDropPaths" $ do
      let testVals =
            [ ( "Int64",
                []
              ),
              ( "Box(Int64)",
                [(ConstTrue,DropPathFetch Nothing)]
              ),
              ( "(a,b)",
                [ (ConstTrue, DropPathSelect (unsafeTy "a") 0 (DropPathFetch (Just "a"))),
                  (ConstTrue, DropPathSelect (unsafeTy "b") 4 (DropPathFetch (Just "b"))),
                  (ConstTrue, DropPathFetch Nothing)
                ]
              ),
              ( "(Int64,b)",
                [ (ConstTrue,DropPathSelect (unsafeTy "b") 8 (DropPathFetch (Just "b"))),
                  (ConstTrue, DropPathFetch Nothing)
                ]
              ),
              (
                "Either(Boolean,(Int32,Int32))",
                [ (Equals [] tyInt32 (PIntLit 1), DropPathSelect (unsafeTy "(Int32,Int32)") 1 (DropPathFetch Nothing)),
                (ConstTrue, DropPathFetch Nothing)
                ]
              )
            ]

      traverse_
        ( \(tyString, paths) -> do
            it (show tyString) $
              evalStateT
                (typeToDropPaths (unsafeTy tyString) ConstTrue id)
                exprState
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
              it (show predicate) $
                evalStateT (predicateToWasm val predicate) exprState
                  `shouldBe` Right expected
          )
          testVals
