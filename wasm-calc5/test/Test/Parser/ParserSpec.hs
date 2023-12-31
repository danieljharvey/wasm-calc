{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.ParserSpec (spec) where

import Calc
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "ParserSpec" $ do
    describe "Type" $ do
      let strings =
            [ ("Boolean", tyBool),
              ("Integer", tyInt),
              ("(Boolean, Boolean, Integer)", tyContainer [tyBool, tyBool, tyInt]),
              ("a", tyVar "a"),
              ("(a,b)", tyContainer [tyVar "a", tyVar "b"]),
              ("Box(a)", tyContainer [tyVar "a"])
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseTypeAndFormatError str of
              Right parsedExp -> parsedExp $> () `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "Module" $ do
      let strings =
            [ ("42", Module [] (int 42)),
              ( "function increment(a: Integer) { a + 1 } 42",
                Module
                  [ Function
                      { fnAnn = (),
                        fnArgs = [("a", TPrim () TInt)],
                        fnFunctionName = "increment",
                        fnBody = EInfix () OpAdd (var "a") (int 1),
                        fnGenerics = mempty
                      }
                  ]
                  (int 42)
              ),
              ( "function increment(a: Integer) { a + 1 } function decrement(a: Integer) { a - 1} 42",
                Module
                  [ Function
                      { fnAnn = (),
                        fnArgs = [("a", TPrim () TInt)],
                        fnFunctionName = "increment",
                        fnBody = EInfix () OpAdd (var "a") (int 1),
                        fnGenerics = mempty
                      },
                    Function
                      { fnAnn = (),
                        fnArgs = [("a", TPrim () TInt)],
                        fnFunctionName = "decrement",
                        fnBody = EInfix () OpSubtract (var "a") (int 1),
                        fnGenerics = mempty
                      }
                  ]
                  (int 42)
              )
            ]

      traverse_
        ( \(str, module') -> it (T.unpack str) $ do
            case parseModuleAndFormatError str of
              Right parsedMod -> parsedMod $> () `shouldBe` module'
              Left e -> error (T.unpack e)
        )
        strings

    describe "Function" $ do
      let strings =
            [ ( "function one() { 1 }",
                Function
                  { fnAnn = (),
                    fnArgs = [],
                    fnFunctionName = "one",
                    fnBody = int 1,
                    fnGenerics = mempty
                  }
              ),
              ( "function sum (a: Integer, b: Integer) { a + b }",
                Function
                  { fnAnn = (),
                    fnArgs = [("a", TPrim () TInt), ("b", TPrim () TInt)],
                    fnFunctionName = "sum",
                    fnBody = EInfix () OpAdd (var "a") (var "b"),
                    fnGenerics = mempty
                  }
              ),
              ( "function pair<a,b>(a: a, b: b) { (a,b) }",
                Function
                  { fnAnn = (),
                    fnArgs = [("a", tyVar "a"), ("b", tyVar "b")],
                    fnFunctionName = "pair",
                    fnBody = ETuple () (var "a") (NE.singleton (var "b")),
                    fnGenerics = ["a", "b"]
                  }
              )
            ]
      traverse_
        ( \(str, fn) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> parsedFn $> () `shouldBe` fn
              Left e -> error (T.unpack e)
        )
        strings

    describe "Expr" $ do
      let strings =
            [ ("-1", int (-1)),
              ("1 + 2", EInfix () OpAdd (int 1) (int 2)),
              ("True", EPrim () (PBool True)),
              ("False", EPrim () (PBool False)),
              ("(1,2,True)", tuple [int 1, int 2, bool True]),
              ( "1 + 2 + 3",
                EInfix
                  ()
                  OpAdd
                  ( EInfix
                      ()
                      OpAdd
                      (int 1)
                      (int 2)
                  )
                  (int 3)
              ),
              ("1 == 2", EInfix () OpEquals (int 1) (int 2)),
              ("1.0 + 1.0", EInfix () OpAdd (float 1.0) (float 1.0)),
              ("if True then 1 else 2", EIf () (bool True) (int 1) (int 2)),
              ("a + 1", EInfix () OpAdd (var "a") (int 1)),
              ("add(1,2)", EApply () "add" [int 1, int 2]),
              ("add(1,2).1", EContainerAccess () (EApply () "add" [int 1, int 2]) 1),
              ("go()", EApply () "go" []),
              ("tuple.1", EContainerAccess () (var "tuple") 1),
              ("Box(1)", EBox () (int 1)),
              ("Box(1).1", EContainerAccess () (box (int 1)) 1),
              ("Box(1)!", EContainerAccess () (box (int 1)) 1),
              ("Box(Box(1)).2.1", EContainerAccess () (EContainerAccess () (box (box (int 1))) 2) 1),
              ("Box(Box(1)).2!", EContainerAccess () (EContainerAccess () (box (box (int 1))) 2) 1)
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseExprAndFormatError str of
              Right parsedExp -> parsedExp $> () `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "Expr with Annotation" $ do
      it "Parses an infix operation with annotations" $ do
        parseExprAndFormatError "20 + 22"
          `shouldBe` Right
            ( EInfix
                (Location 0 7)
                OpAdd
                (EPrim (Location 0 2) (PInt 20))
                (EPrim (Location 5 7) (PInt 22))
            )
