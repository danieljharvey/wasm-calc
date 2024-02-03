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
              ("Int64", tyInt64),
              ("Void", TPrim () TVoid),
              ("(Boolean, Boolean, Int64)", tyContainer [tyBool, tyBool, tyInt64]),
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
            [ ( "export function increment(a: Int64) -> Int64 { a + 1 }",
                Module
                  { mdFunctions =
                      [ Function
                          { fnPublic = True,
                            fnAnn = (),
                            fnArgs =
                              [ FunctionArg
                                  { faName = "a",
                                    faType = tyInt64,
                                    faAnn = ()
                                  }
                              ],
                            fnFunctionName = "increment",
                            fnBody = EInfix () OpAdd (var "a") (int 1),
                            fnGenerics = mempty,
                            fnReturnType = tyInt64
                          }
                      ],
                    mdImports = [],
                    mdMemory = Nothing
                  }
              ),
              ( "function increment(a: Int64) -> Int64 { a + 1 } function decrement(a: Int64) -> Int64 { a - 1}",
                Module
                  { mdFunctions =
                      [ Function
                          { fnPublic = False,
                            fnAnn = (),
                            fnArgs =
                              [ FunctionArg
                                  { faName = "a",
                                    faType = tyInt64,
                                    faAnn = ()
                                  }
                              ],
                            fnFunctionName = "increment",
                            fnBody = EInfix () OpAdd (var "a") (int 1),
                            fnGenerics = mempty,
                            fnReturnType = tyInt64
                          },
                        Function
                          { fnPublic = False,
                            fnAnn = (),
                            fnArgs =
                              [ FunctionArg
                                  { faName = "a",
                                    faType = tyInt64,
                                    faAnn = ()
                                  }
                              ],
                            fnFunctionName = "decrement",
                            fnBody = EInfix () OpSubtract (var "a") (int 1),
                            fnGenerics = mempty,
                            fnReturnType = tyInt64
                          }
                      ],
                    mdImports = mempty,
                    mdMemory = Nothing
                  }
              ),
              ( joinLines
                  [ "import maths.add as add(a: Int64, b:Int64) -> Int64"
                  ],
                Module
                  { mdFunctions = [],
                    mdImports =
                      [ Import
                          { impAnn = (),
                            impArgs =
                              [ ImportArg {iaAnn = (), iaName = "a", iaType = tyInt64},
                                ImportArg {iaAnn = (), iaName = "b", iaType = tyInt64}
                              ],
                            impImportName = "add",
                            impReturnType = tyInt64,
                            impExternalModule = "maths",
                            impExternalFunction = "add"
                          }
                      ],
                    mdMemory = Nothing
                  }
              ),
              ( joinLines
                  [ "memory 100"
                  ],
                Module
                  { mdFunctions = [],
                    mdImports = [],
                    mdMemory = Just 100
                  }
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
            [ ( "function one() -> Int64 { 1 }",
                Function
                  { fnPublic = False,
                    fnAnn = (),
                    fnArgs = [],
                    fnFunctionName = "one",
                    fnBody = int 1,
                    fnGenerics = mempty,
                    fnReturnType = tyInt64
                  }
              ),
              ( "function sum (a: Int64, b: Int64) -> Int64 { a + b }",
                Function
                  { fnPublic = False,
                    fnAnn = (),
                    fnArgs =
                      [ FunctionArg
                          { faName = "a",
                            faType = tyInt64,
                            faAnn = ()
                          },
                        FunctionArg
                          { faName = "b",
                            faType = tyInt64,
                            faAnn = ()
                          }
                      ],
                    fnFunctionName = "sum",
                    fnBody = EInfix () OpAdd (var "a") (var "b"),
                    fnGenerics = mempty,
                    fnReturnType = tyInt64
                  }
              ),
              ( "function pair<a,b>(a: a, b: b) -> (a,b) { (a,b) }",
                Function
                  { fnPublic = False,
                    fnAnn = (),
                    fnArgs =
                      [ FunctionArg
                          { faName = "a",
                            faType = tyVar "a",
                            faAnn = ()
                          },
                        FunctionArg {faName = "b", faType = tyVar "b", faAnn = ()}
                      ],
                    fnFunctionName = "pair",
                    fnBody = ETuple () (var "a") (NE.singleton (var "b")),
                    fnGenerics = ["a", "b"],
                    fnReturnType = tyContainer [tyVar "a", tyVar "b"]
                  }
              ),
              ( "function horse() -> Int64 { let a = 100; a }",
                Function
                  { fnPublic = False,
                    fnAnn = (),
                    fnArgs = [],
                    fnFunctionName = "horse",
                    fnBody = ELet () (PVar () "a") (int 100) (var "a"),
                    fnGenerics = [],
                    fnReturnType = tyInt64
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

    describe "Pattern" $ do
      let strings =
            [ ("_", PWildcard ()),
              ("a", PVar () "a"),
              ("Box(_)", PBox () (PWildcard ()))
            ]
      traverse_
        ( \(str, pat) -> it (T.unpack str) $ do
            case parsePatternAndFormatError str of
              Right parsedPattern -> parsedPattern $> () `shouldBe` pat
              Left e -> error (T.unpack e)
        )
        strings

    describe "Expr" $ do
      let strings =
            [ ("-1", int (fromIntegral (-1 :: Integer))),
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
              ("Box(Box(1)).2!", EContainerAccess () (EContainerAccess () (box (box (int 1))) 2) 1),
              ("let a = 100; a", ELet () (PVar () "a") (int 100) (var "a")),
              ( "let (a,b) = (1,2); a + b",
                ELet
                  ()
                  (PTuple () (PVar () "a") (NE.singleton $ PVar () "b"))
                  (tuple [int 1, int 2])
                  (EInfix () OpAdd (var "a") (var "b"))
              ),
              ("dogs(); 100", ELet () (PWildcard ()) (EApply () "dogs" []) (int 100)),
              ("100; 100", ELet () (PWildcard ()) (int 100) (int 100)),
              ("(100 : Int32)", EAnn () tyInt32 (int 100))
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
                (EPrim (Location 0 2) (PIntLit 20))
                (EPrim (Location 5 7) (PIntLit 22))
            )
