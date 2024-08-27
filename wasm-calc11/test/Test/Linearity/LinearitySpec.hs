{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Linearity.LinearitySpec (spec) where

import Calc
import Calc.Linearity
import Calc.Typecheck
import Control.Monad (void)
import Data.Either (isRight)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

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

spec :: Spec
spec = do
  describe "LinearitySpec" $ do
    describe "decorate" $ do
      let dVar = EVar Nothing
          dBool = EPrim Nothing . PBool
          dTyInt32 = TPrim Nothing TInt32
          dTyInt64 = TPrim Nothing TInt64
          dInt = EPrim Nothing . PIntLit
          dTuple = \case
            (a : b : rest) -> ETuple Nothing a (b NE.:| rest)
            _ -> error "not enough items for tuple"
          tyTuple as = TContainer mempty (NE.fromList as)
          tyInt32 = TPrim mempty TInt32
          tyInt64 = TPrim mempty TInt64
          letAEqualsTuple =
            ELet
              Nothing
              (PVar Nothing "a")
              ( ETuple
                  Nothing
                  (EAnn Nothing dTyInt32 (dInt 1))
                  (NE.singleton $ EAnn Nothing dTyInt32 (dInt 2))
              )
          dropIdents ids = Just $ DropIdentifiers (NE.fromList ids)

      let strings =
            [ ( "function valueSometimesUsed() -> Int32 { let a: Int32 = 1; if True then a else 2 }",
                ELet
                  Nothing
                  (PVar Nothing "a")
                  (EAnn Nothing dTyInt32 (dInt 1))
                  (EIf Nothing (dBool True) (dVar "a") (dInt 2))
              ),
              ( "function dropTupleAfterUse() -> Int64 { let (a,_) = ((100: Int64),(200: Int64)); a }",
                ELet
                  Nothing
                  (PTuple (Just DropMe) (PVar Nothing "a") (NE.singleton $ PVar Nothing "_fresh_name1"))
                  ( ETuple
                      Nothing
                      (EAnn Nothing dTyInt64 (dInt 100))
                      (NE.singleton $ EAnn Nothing dTyInt64 (dInt 200))
                  )
                  (dVar "a")
              ),
              ( "function allocUnused() -> Int64 { let _ = Box((1: Int32)); 22 }",
                ELet
                  Nothing
                  (PVar (Just DropMe) "_fresh_name1")
                  (EBox Nothing (EAnn Nothing dTyInt32 (dInt 1)))
                  (dInt 22)
              ),
              ( "function incrementallyDropBoxesAfterUse() -> Int64 { let Box(outer) = Box(Box((100: Int64))); let Box(inner) = outer; inner }",
                ELet
                  Nothing
                  (PBox (Just DropMe) (PVar Nothing "outer"))
                  (EBox Nothing (EBox Nothing (EAnn Nothing dTyInt64 (dInt 100))))
                  ( ELet
                      Nothing
                      (PBox (Just DropMe) (PVar Nothing "inner"))
                      (dVar "outer")
                      (dVar "inner")
                  )
              ),
              ( "function tupleSometimesUsed() -> (Int32,Int32) { let a = ((1: Int32), (2: Int32)); let b = ((2: Int32), (3: Int32)); if True then a else b}",
                letAEqualsTuple
                  ( ELet
                      Nothing
                      (PVar Nothing "b")
                      ( dTuple
                          [ EAnn Nothing dTyInt32 (dInt 2),
                            EAnn Nothing dTyInt32 (dInt 3)
                          ]
                      )
                      ( EIf
                          Nothing
                          (dBool True)
                          (EVar (dropIdents [("b", tyTuple [tyInt32, tyInt32])]) "a")
                          (EVar (dropIdents [("a", tyTuple [tyInt32, tyInt32])]) "b")
                      )
                  )
              ),
              ( "function dropAfterDestructure() -> Int32 { let a = ((1: Int32), (2: Int32)); let (b,c) = a; b + c }",
                letAEqualsTuple
                  ( ELet
                      Nothing
                      (PTuple (Just DropMe) (PVar Nothing "b") (NE.singleton (PVar Nothing "c")))
                      (dVar "a")
                      (EInfix Nothing OpAdd (dVar "b") (dVar "c"))
                  )
              ),
              ( "function dropVariablesInIfBranches() -> Int64 { let a = Box((1: Int64)); let b = Box((2: Int64)); let Box(c) = if True then a else b; c }",
                ELet
                  Nothing
                  (PVar Nothing "a")
                  (EBox Nothing (EAnn Nothing dTyInt64 (dInt 1)))
                  ( ELet
                      Nothing
                      (PVar Nothing "b")
                      (EBox Nothing (EAnn Nothing dTyInt64 (dInt 2)))
                      ( ELet
                          Nothing
                          (PBox (Just DropMe) (PVar Nothing "c"))
                          ( EIf
                              Nothing
                              (dBool True)
                              (EVar (dropIdents [("b", tyTuple [tyInt64])]) "a")
                              (EVar (dropIdents [("a", tyTuple [tyInt64])]) "b")
                          )
                          (dVar "c")
                      )
                  )
              ),
              ( "function hmm() -> Int64 { let a = ((1: Int64), (2: Int64)); let (b,c) = a; b + c }",
                ELet
                  Nothing
                  (PVar Nothing "a")
                  ( ETuple
                      Nothing
                      (EAnn Nothing (TPrim Nothing TInt64) (EPrim Nothing (PIntLit 1)))
                      (NE.singleton $ EAnn Nothing (TPrim Nothing TInt64) (EPrim Nothing (PIntLit 2)))
                  )
                  ( ELet
                      Nothing
                      (PTuple (Just DropMe) (PVar Nothing "b") (NE.singleton $ PVar Nothing "c"))
                      (EVar Nothing "a")
                      (EInfix Nothing OpAdd (EVar Nothing "b") (EVar Nothing "c"))
                  )
              ),
              ( "function fst<a,b>(pair: (a,b)) -> Box(a) { let (a, _) = pair; Box(a) }",
                ELet
                  Nothing
                  (PTuple (Just DropMe) (PVar Nothing "a") (NE.singleton $ PVar (Just DropMe) "_fresh_name1"))
                  (EVar Nothing "pair")
                  (EBox Nothing (EVar Nothing "a"))
              ),
              ( "function matchBool<a>(one: a, two: a) -> a { case True { True -> one, False -> two } }",
                EMatch
                  Nothing
                  (dBool True)
                  ( NE.fromList
                      [ (PLiteral Nothing (PBool True), EVar (dropIdents [("two", TVar () "a")]) "one"),
                        (PLiteral Nothing (PBool False), EVar (dropIdents [("one", TVar () "a")]) "two")
                      ]
                  )
              ),
              ( "function matchInts<a>(one: a, two: a, three: a) -> a { case (1: Int32) { 1 -> one, 2 -> two, _ -> three } }",
                EMatch
                  Nothing
                  (EAnn Nothing dTyInt32 (dInt 1))
                  ( NE.fromList
                      [ (PLiteral Nothing (PIntLit 1), EVar (dropIdents [("three", TVar () "a"), ("two", TVar () "a")]) "one"),
                        (PLiteral Nothing (PIntLit 2), EVar (dropIdents [("one", TVar () "a"), ("three", TVar () "a")]) "two"),
                        (PVar Nothing "_fresh_name1", EVar (dropIdents [("one", TVar () "a"), ("two", TVar () "a")]) "three")
                      ]
                  )
              ),
              ( "function matchWithBox() -> Int64 { case True { True -> { let box = Box((100: Int64)); let Box(b) = box; b} , False -> 0 } }",
                EMatch
                  Nothing
                  (dBool True)
                  ( NE.fromList
                      [ ( PLiteral Nothing (PBool True),
                          EBlock
                            Nothing
                            ( ELet
                                Nothing
                                (PVar Nothing "box")
                                ( EBox
                                    Nothing
                                    ( EAnn
                                        Nothing
                                        (TPrim Nothing TInt64)
                                        (EPrim Nothing (PIntLit 100))
                                    )
                                )
                                ( ELet
                                    Nothing
                                    (PBox (Just DropMe) (PVar Nothing "b"))
                                    (EVar Nothing "box")
                                    (EVar Nothing "b")
                                )
                            )
                        ),
                        (PLiteral Nothing (PBool False), dInt 0)
                      ]
                  )
              )
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> do
                case runTC (elaborateFunction parsedFn) of
                  Left e -> error (show e)
                  Right typedFn ->
                    let !result = (snd . (fmap . fmap) void <$> fst (getFunctionUses typedFn))
                     in result `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "getFunctionUses" $ do
      let strings =
            [ ( "function sum (a: Int64, b: Int64) -> Int64 { a + b }",
                LinearState
                  { lsVars =
                      M.fromList [(UserDefined "a", (LTPrimitive, ())), (UserDefined "b", (LTPrimitive, ()))],
                    lsUses = NE.singleton (M.fromList [("b", NE.singleton $ Whole ()), ("a", NE.singleton $ Whole ())]),
                    lsFresh = 0
                  }
              ),
              ( "function pair<a,b>(a: a, b: b) -> (a,b) { (a,b) }",
                LinearState
                  { lsVars = M.fromList [(UserDefined "a", (LTBoxed, ())), (UserDefined "b", (LTBoxed, ()))],
                    lsUses =
                      NE.singleton
                        ( M.fromList
                            [ ("b", NE.singleton $ Whole ()),
                              ("a", NE.singleton $ Whole ())
                            ]
                        ),
                    lsFresh = 0
                  }
              ),
              ( "function dontUseA<a,b>(a: a, b: b) -> b { b }",
                LinearState
                  { lsVars = M.fromList [(UserDefined "a", (LTBoxed, ())), (UserDefined "b", (LTBoxed, ()))],
                    lsUses = NE.singleton (M.fromList [("b", NE.singleton $ Whole ())]),
                    lsFresh = 0
                  }
              ),
              ( "function dup<a>(a: a) -> (a,a) { (a,a)}",
                LinearState
                  { lsVars = M.fromList [(UserDefined "a", (LTBoxed, ()))],
                    lsUses = NE.singleton (M.fromList [("a", NE.fromList [Whole (), Whole ()])]),
                    lsFresh = 0
                  }
              )
            ]
      traverse_
        ( \(str, linearState) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> do
                case runTC (elaborateFunction parsedFn) of
                  Left e -> error (show e)
                  Right typedFn ->
                    void (snd $ getFunctionUses typedFn) `shouldBe` linearState
              Left e -> error (T.unpack e)
        )
        strings

    describe "validateFunction" $ do
      describe "expected successes" $ do
        let success =
              [ "function sum (a: Int64, b: Int64) -> Int64 { a + b }",
                "function pair<a,b>(a: a, b: b) -> (a,b) { (a,b) }",
                "function addPair(pair: (Int64,Int64)) -> Int64 { let (a,b) = pair; a + b }",
                "function fst<a,b>(pair: (a,b)) -> Box(a) { let (a,_) = pair; Box(a) }",
                "function main() -> Int64 { let _ = (1: Int64); 2 }",
                "function bothSidesOfIf() -> (Boolean,Boolean) { let pair = (True,False); if True then pair else pair }",
                "function bothSidesOfMatch() -> (Boolean,Boolean) { let pair = (True,False); case True { True -> pair, False -> pair } }"
              ]
        traverse_
          ( \str -> it (T.unpack str) $ do
              case parseFunctionAndFormatError str of
                Right parsedFn -> do
                  case runTC (elaborateFunction parsedFn) of
                    Left e -> error (show e)
                    Right typedFn ->
                      validateFunction typedFn `shouldSatisfy` isRight
                Left e -> error (T.unpack e)
          )
          success

      describe "expected failures" $ do
        let failures =
              [ ( "function dontUseA<a,b>(a: a, b: b) -> b { b }",
                  NotUsed () "a"
                ),
                ( "function dontUsePrimA(a: Int64, b: Int64) -> Int64 { b }",
                  NotUsed () "a"
                ),
                ( "function dup<a>(a: a) -> (a,a) { (a,a)}",
                  UsedMultipleTimes (NE.fromList [(), ()]) "a"
                ),
                ( "function withPair<a,b>(pair: (a,b)) -> (a,a,b) { let (a,b) = pair; (a, a, b) }",
                  UsedMultipleTimes (NE.fromList [(), ()]) "a"
                ),
                ( "function bothSidesOfIf() -> (Boolean,Boolean) { let pair = (True,False); if True then { let _ = pair; pair } else pair }",
                  UsedMultipleTimes (NE.fromList [(), ()]) "pair"
                )
              ]
        traverse_
          ( \(str, err) -> it (T.unpack str) $ do
              case parseFunctionAndFormatError str of
                Right parsedFn -> do
                  case runTC (elaborateFunction (void parsedFn)) of
                    Left e -> error (show e)
                    Right typedFn ->
                      validateFunction typedFn `shouldBe` Left err
                Left e -> error (T.unpack e)
          )
          failures
