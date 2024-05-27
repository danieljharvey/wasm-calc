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
runTC = runTypecheckM (TypecheckEnv mempty mempty 0)

spec :: Spec
spec = do
  describe "LinearitySpec" $ do
    fdescribe "decorate" $ do
      let dVar = EVar Nothing
          dBool = EPrim Nothing . PBool
          dTyInt32 = TPrim Nothing TInt32
          dTyInt64 = TPrim Nothing TInt64
          dInt = EPrim Nothing . PIntLit
          dTuple = \case
            (a : b : rest) -> ETuple Nothing a (b NE.:| rest)
            _ -> error "not enough items for tuple"
          tyBox a = TContainer mempty (NE.singleton a)
          tyTuple as = TContainer mempty (NE.fromList as)
          tyInt32 = TPrim mempty TInt32
          tyInt64 = TPrim mempty TInt64
      let letAEqualsTuple =
            ELet
              Nothing
              (PVar Nothing "a")
              ( ETuple
                  Nothing
                  (EAnn Nothing dTyInt32 (dInt 1))
                  (NE.singleton $ EAnn Nothing dTyInt32 (dInt 2))
              )
      let dropIdents ids = Just $ DropIdentifiers (NE.fromList ids)

      let strings =
            [ ( "function valueSometimesUsed() -> Int32 { let a: Int32 = 1; if True then a else 2 }",
                ELet
                  Nothing
                  (PVar Nothing "a")
                  (EAnn Nothing dTyInt32 (dInt 1))
                  (EIf Nothing (dBool True) (dVar "a") (dInt 2))
              ),
              ( "function dropBoxAfterUse() -> Int64 { let Box(a) = Box((100: Int64)); a }",
                ELet
                  Nothing
                  (PBox Nothing (PVar Nothing "a"))
                  (EBox (Just DropMe) (EAnn Nothing dTyInt64 (dInt 100)))
                  (dVar "a")
              ),
              ( "function incrementallyDropBoxesAfterUse() -> Int64 { let Box(outer) = Box(Box((100: Int64))); let Box(inner) = outer; inner }",
                ELet
                  Nothing
                  (PBox Nothing (PVar Nothing "outer"))
                  (EBox (Just DropMe) (EBox Nothing (EAnn Nothing dTyInt64 (dInt 100))))
                  ( ELet
                      (dropIdents [("outer", tyBox tyInt64)])
                      (PBox Nothing (PVar Nothing "inner"))
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
                      (dropIdents [("a", tyTuple [tyInt32, tyInt32])])
                      (PTuple Nothing (PVar Nothing "b") (NE.singleton (PVar Nothing "c")))
                      (dVar "a")
                      (EInfix Nothing OpAdd (dVar "b") (dVar "c"))
                  )
              ) {-,
                ( "function dropAfterDestructureWithTransfer() -> Int32 { let a = ((1: Int32), (2: Int32)); let b = a; let (c,d) = b; c + d }",
                  letAEqualsTuple
                    ( ELet
                        Nothing
                        (PVar Nothing "b")
                        (dVar "a")
                        ( ELet
                            (dropIdents ["a"])
                            (PTuple Nothing (PVar Nothing "c") (NE.singleton (PVar Nothing "d")))
                            (dVar "b")
                            (EInfix Nothing OpAdd (dVar "c") (dVar "d"))
                        )
                    )
                )-}
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> do
                case runTC (elaborateFunction parsedFn) of
                  Left e -> error (show e)
                  Right typedFn ->
                    snd . (fmap . fmap) void <$> fst (getFunctionUses typedFn) `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "getFunctionUses" $ do
      let strings =
            [ ( "function sum (a: Int64, b: Int64) -> Int64 { a + b }",
                LinearState
                  { lsVars =
                      M.fromList [("a", (LTPrimitive, ())), ("b", (LTPrimitive, ()))],
                    lsUses = [("b", Whole ()), ("a", Whole ())],
                    lsFresh = 0
                  }
              ),
              ( "function pair<a,b>(a: a, b: b) -> (a,b) { (a,b) }",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ())), ("b", (LTBoxed, ()))],
                    lsUses = [("b", Whole ()), ("a", Whole ())],
                    lsFresh = 0
                  }
              ),
              ( "function dontUseA<a,b>(a: a, b: b) -> b { b }",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ())), ("b", (LTBoxed, ()))],
                    lsUses = [("b", Whole ())],
                    lsFresh = 0
                  }
              ),
              ( "function dup<a>(a: a) -> (a,a) { (a,a)}",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ()))],
                    lsUses = [("a", Whole ()), ("a", Whole ())],
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
                "function main() -> Int64 { let _ = (1: Int64); 2 }"
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
              [ {-( "function dontUseA<a,b>(a: a, b: b) -> b { b }",
                  NotUsed () "a"
                ),
                ( "function dontUsePrimA(a: Int64, b: Int64) -> Int64 { b }",
                  NotUsed () "a"
                ),-}
                ( "function dup<a>(a: a) -> (a,a) { (a,a)}",
                  UsedMultipleTimes [(), ()] "a"
                ),
                {-( "function twice(pair: (Int64, Int64)) { pair.1 + pair.2 }",
                  UsedMultipleTimes "pair"
                ),-}
                ( "function withPair<a,b>(pair: (a,b)) -> (a,a,b) { let (a,b) = pair; (a, a, b) }",
                  UsedMultipleTimes [(), ()] "a"
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
