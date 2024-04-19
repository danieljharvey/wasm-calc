{-# LANGUAGE OverloadedStrings #-}

module Test.Linearity.LinearitySpec (spec) where

import           Calc
import           Calc.Linearity
import           Calc.Typecheck
import           Control.Monad      (void)
import           Data.Either        (isRight)
import           Data.Foldable      (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import           Test.Helpers
import           Test.Hspec

runTC :: TypecheckM ann a -> Either (TypeError ann) a
runTC = runTypecheckM (TypecheckEnv mempty mempty 0)

spec :: Spec
spec = do
  describe "LinearitySpec" $ do
    fdescribe "decorate" $ do
      let letAEqualsTuple =
            ELet
              mempty
              (PVar mempty "a")
              ( tuple
                  [ EAnn [] tyInt32 (int 1),
                    EAnn [] tyInt32 (int 2)
                  ]
              )

      let strings =
            [ ( "function tuple() -> (Int32,Int32) { let a = ((1: Int32), (2: Int32)); a }",
                letAEqualsTuple
                  (var "a")
              ),
              ( "function valueSometimesUsed() -> Int32 { let a: Int32 = 1; if True then a else 2 }",
                ELet mempty (PVar [] "a") (EAnn [] tyInt32 (int 1)) (EIf mempty (bool True) (var "a") (int 2))),

              ( "function tupleSometimesUsed() -> (Int32,Int32) { let a = ((1: Int32), (2: Int32)); let b = ((2: Int32), (3: Int32)); if True then a else b}",
                letAEqualsTuple
                  ( ELet
                      mempty
                      (PVar [] "b")
                      ( tuple
                          [ EAnn [] tyInt32 (int 2),
                            EAnn [] tyInt32 (int 3)
                          ]
                      )
                      (EIf mempty (bool True) (EVar [DropIdentifier "b"] "a") (EVar [DropIdentifier "a"] "b"))
                  )
              ),
              ( "function dropAfterDestructure() -> Int32 { let a = ((1: Int32), (2: Int32)); let (b,c) = a; b + c }",
                letAEqualsTuple
                  ( ELet
                      [DropIdentifier "a"]
                      (PTuple [] (PVar [] "b") (NE.singleton (PVar [] "c")))
                      (var "a")
                      (EInfix mempty OpAdd (var "b") (var "c"))
                  )
              ),
              ( "function dropAfterDestructureWithTransfer() -> Int32 { let a = ((1: Int32), (2: Int32)); let b = a; let (c,d) = b; c + d }",
                letAEqualsTuple
                  ( ELet
                      []
                      (PVar [] "b")
                      (var "a")
                      ( ELet
                          [DropIdentifier "b"]
                          (PTuple [] (PVar [] "c") (NE.singleton (PVar [] "d")))
                          (var "b")
                          (EInfix mempty OpAdd (var "c") (var "d"))
                      )
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
                    fst (getFunctionUses typedFn) `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "getFunctionUses" $ do
      let strings =
            [ ( "function sum (a: Int64, b: Int64) -> Int64 { a + b }",
                LinearState
                  { lsVars =
                      M.fromList [("a", (LTPrimitive, ())), ("b", (LTPrimitive, ()))],
                    lsUses = [("b", Whole ()), ("a", Whole ())]
                  }
              ),
              ( "function pair<a,b>(a: a, b: b) -> (a,b) { (a,b) }",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ())), ("b", (LTBoxed, ()))],
                    lsUses = [("b", Whole ()), ("a", Whole ())]
                  }
              ),
              ( "function dontUseA<a,b>(a: a, b: b) -> b { b }",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ())), ("b", (LTBoxed, ()))],
                    lsUses = [("b", Whole ())]
                  }
              ),
              ( "function dup<a>(a: a) -> (a,a) { (a,a)}",
                LinearState
                  { lsVars = M.fromList [("a", (LTBoxed, ()))],
                    lsUses = [("a", Whole ()), ("a", Whole ())]
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
              [ ( "function dontUseA<a,b>(a: a, b: b) -> b { b }",
                  NotUsed () "a"
                ),
                ( "function dontUsePrimA(a: Int64, b: Int64) -> Int64 { b }",
                  NotUsed () "a"
                ),
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
