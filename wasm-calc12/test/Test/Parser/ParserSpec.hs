{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.ParserSpec (spec) where

import Calc
import Calc.Module
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Test.Helpers
import Test.Hspec

emptyFunction :: Function ()
emptyFunction =
  Function
    { fnPublic = False,
      fnAnn = (),
      fnArgs = [],
      fnFunctionName = "",
      fnBody = bool True,
      fnGenerics = mempty,
      fnReturnType = tyInt64,
      fnAbilityConstraints = mempty
    }

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
              ("Box(a)", tyContainer [tyVar "a"]),
              ("Colour", tyConstructor "Colour" mempty),
              ("Maybe(a)", tyConstructor "Maybe" [tyVar "a"]),
              ( "Fn() -> Int32",
                TFunction () mempty tyInt32
              ),
              ( "Fn(Boolean,Int64) -> Int32",
                TFunction () [tyBool, tyInt64] tyInt32
              )
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseTypeAndFormatError str of
              Right parsedExp -> parsedExp $> () `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "Module" $ do
      let emptyModule =
            Module
              { mdFunctions = mempty,
                mdImports = mempty,
                mdMemory = Nothing,
                mdGlobals = mempty,
                mdTests = mempty,
                mdDataTypes = mempty
              }
      let strings =
            [ ( "export function increment(a: Int64) -> Int64 { a + 1 }",
                emptyModule
                  { mdFunctions =
                      [ emptyFunction
                          { fnPublic = True,
                            fnArgs =
                              [ FunctionArg
                                  { faName = "a",
                                    faType = tyInt64,
                                    faAnn = ()
                                  }
                              ],
                            fnFunctionName = "increment",
                            fnBody = EInfix () OpAdd (var "a") (int 1),
                            fnReturnType = tyInt64
                          }
                      ]
                  }
              ),
              ( "function increment(a: Int64) -> Int64 { a + 1 } function decrement(a: Int64) -> Int64 { a - 1}",
                emptyModule
                  { mdFunctions =
                      [ emptyFunction
                          { fnArgs =
                              [ FunctionArg
                                  { faName = "a",
                                    faType = tyInt64,
                                    faAnn = ()
                                  }
                              ],
                            fnFunctionName = "increment",
                            fnBody = EInfix () OpAdd (var "a") (int 1),
                            fnReturnType = tyInt64
                          },
                        emptyFunction
                          { fnArgs =
                              [ FunctionArg
                                  { faName = "a",
                                    faType = tyInt64,
                                    faAnn = ()
                                  }
                              ],
                            fnFunctionName = "decrement",
                            fnBody = EInfix () OpSubtract (var "a") (int 1),
                            fnReturnType = tyInt64
                          }
                      ]
                  }
              ),
              ( joinLines
                  [ "import maths.add as add(a: Int64, b:Int64) -> Int64"
                  ],
                emptyModule
                  { mdImports =
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
                      ]
                  }
              ),
              ( joinLines
                  [ "memory 100"
                  ],
                emptyModule
                  { mdMemory = Just (LocalMemory () 100)
                  }
              ),
              ( joinLines
                  [ "memory 100",
                    "function main() -> Int32 { 100 }"
                  ],
                emptyModule
                  { mdFunctions =
                      [ emptyFunction
                          { fnFunctionName = "main",
                            fnBody = int 100,
                            fnReturnType = tyInt32
                          }
                      ],
                    mdMemory = Just (LocalMemory () 100)
                  }
              ),
              ( joinLines
                  [ "import env.memory as memory 100"
                  ],
                emptyModule
                  { mdMemory = Just (ImportedMemory () "env" "memory" 100)
                  }
              ),
              ( joinLines
                  [ "global one: Int64 = 1",
                    "global true = True",
                    "global mut counter: Int64 = 0"
                  ],
                emptyModule
                  { mdGlobals =
                      [ Global
                          { glbAnn = (),
                            glbMutability = Constant,
                            glbIdentifier = "one",
                            glbExpr = EAnn () tyInt64 (int 1)
                          },
                        Global
                          { glbAnn = (),
                            glbMutability = Constant,
                            glbIdentifier = "true",
                            glbExpr = bool True
                          },
                        Global
                          { glbAnn = (),
                            glbMutability = Mutable,
                            glbIdentifier = "counter",
                            glbExpr = EAnn () tyInt64 (int 0)
                          }
                      ]
                  }
              ),
              ( joinLines
                  [ "global mut counter: Int64 = 0",
                    "function main() -> Void { set(counter, 1) }"
                  ],
                emptyModule
                  { mdGlobals =
                      [ Global
                          { glbAnn = (),
                            glbMutability = Mutable,
                            glbIdentifier = "counter",
                            glbExpr = EAnn () tyInt64 (int 0)
                          }
                      ],
                    mdFunctions =
                      [ emptyFunction
                          { fnFunctionName = "main",
                            fnReturnType = tyVoid,
                            fnBody = ESet () "counter" (int 1)
                          }
                      ]
                  }
              ),
              ( joinLines
                  ["test itsTrue = True"],
                emptyModule
                  { mdTests =
                      [ Test
                          { tesAnn = (),
                            tesName = "itsTrue",
                            tesExpr = bool True
                          }
                      ]
                  }
              ),
              ( joinLines ["type Maybe<a> = Just(a) | Nothing"],
                emptyModule
                  { mdDataTypes =
                      [ Data
                          { dtName = DataName "Maybe",
                            dtVars = ["a"],
                            dtConstructors =
                              M.fromList
                                [ ("Just", [TVar mempty "a"]),
                                  ("Nothing", mempty)
                                ]
                          }
                      ]
                  }
              )
            ]

      traverse_
        ( \(str, module') -> it (T.unpack str) $ do
            case parseModuleAndFormatError str of
              Left e -> error (T.unpack e)
              Right parsedModuleItems ->
                case resolveModule parsedModuleItems of
                  Left e -> error (show e)
                  Right parsedMod ->
                    parsedMod $> () `shouldBe` module'
        )
        strings

    describe "Data" $ do
      let strings =
            [ ( "type Colour = Red | Green | Blue",
                Data
                  { dtName = DataName "Colour",
                    dtVars = mempty,
                    dtConstructors =
                      M.fromList
                        [ ("Red", mempty),
                          ("Green", mempty),
                          ("Blue", mempty)
                        ]
                  }
              ),
              ( "type Void",
                Data {dtName = DataName "Void", dtVars = mempty, dtConstructors = mempty}
              ),
              ( "type Proxy<a> = Proxy",
                Data
                  { dtName = DataName "Proxy",
                    dtVars = ["a"],
                    dtConstructors = M.singleton "Proxy" mempty
                  }
              ),
              ( "type Either<e,a> = Left(e) | Right(a)",
                Data
                  { dtName = DataName "Either",
                    dtVars = ["e", "a"],
                    dtConstructors =
                      M.fromList
                        [ ("Left", [TVar mempty "e"]),
                          ("Right", [TVar mempty "a"])
                        ]
                  }
              ),
              ( "type These<a,b> = This(a) | That(b) | These(a,b)",
                Data
                  { dtName = DataName "These",
                    dtVars = ["a", "b"],
                    dtConstructors =
                      M.fromList
                        [ ("This", [TVar mempty "a"]),
                          ("That", [TVar mempty "b"]),
                          ("These", [TVar mempty "a", TVar mempty "b"])
                        ]
                  }
              )
            ]
      traverse_
        ( \(str, dt) -> it (T.unpack str) $ do
            case parseDataAndFormatError str of
              Right parsedData -> parsedData $> () `shouldBe` dt
              Left e -> error (T.unpack e)
        )
        strings

    describe "Function" $ do
      let strings =
            [ ( "function one() -> Int64 { 1 }",
                emptyFunction
                  { fnFunctionName = "one",
                    fnBody = int 1,
                    fnReturnType = tyInt64
                  }
              ),
              ( "function sum (a: Int64, b: Int64) -> Int64 { a + b }",
                emptyFunction
                  { fnArgs =
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
                    fnReturnType = tyInt64
                  }
              ),
              ( "function pair<a,b>(a: a, b: b) -> (a,b) { (a,b) }",
                emptyFunction
                  { fnArgs =
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
                emptyFunction
                  { fnFunctionName = "horse",
                    fnBody = ELet () (PVar () "a") (int 100) (var "a"),
                    fnReturnType = tyInt64
                  }
              ),
              ( "function [] horse() -> Int64 { 1 }",
                emptyFunction
                  { fnFunctionName = "horse",
                    fnBody = int 1,
                    fnReturnType = tyInt64
                  }
              ),
              ( "function [noallocate] horse() -> Int64 { 1 }",
                emptyFunction
                  { fnFunctionName = "horse",
                    fnBody = int 1,
                    fnReturnType = tyInt64,
                    fnAbilityConstraints = S.singleton NoAllocate
                  }
              ),
              ( "function [noallocate noglobalmutate noimports] horse() -> Int64 { 1 }",
                emptyFunction
                  { fnFunctionName = "horse",
                    fnBody = int 1,
                    fnReturnType = tyInt64,
                    fnAbilityConstraints = S.fromList [NoAllocate, NoGlobalMutate, NoImports]
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
              ("Box(_)", PBox () (PWildcard ())),
              ("1", patInt 1),
              ("Just(True)", PConstructor () "Just" [patBool True]),
              ("These(True,False)", PConstructor () "These" [patBool True, patBool False]),
              ("These(1,2)", PConstructor () "These" [patInt 1, patInt 2])
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
              ("True && True", EInfix () OpAnd (bool True) (bool True)),
              ("True || False", EInfix () OpOr (bool True) (bool False)),
              ("if True then 1 else 2", EIf () (bool True) (int 1) (int 2)),
              ("a + 1", EInfix () OpAdd (var "a") (int 1)),
              ("add(1,2,)", EApply () "add" [int 1, int 2]),
              ("go()", EApply () "go" []),
              ("Box(1)", EBox () (int 1)),
              ( "\\() -> Int32 { 3 }",
                ELambda () mempty tyInt32 (int 3)
              ),
              ( "\\(a: Int32,b: Boolean) -> Int32 { 3 }",
                ELambda () [("a", tyInt32), ("b", tyBool)] tyInt32 (int 3)
              ),
              ( "let f = \\() -> Boolean { True }; f",
                ELet
                  ()
                  (PVar () "f")
                  ( ELambda
                      ()
                      []
                      tyBool
                      (bool True)
                  )
                  (var "f")
              ),
              ( "let f = \\() -> Boolean { let a = True; a }; f",
                ELet
                  ()
                  (PVar () "f")
                  ( ELambda
                      ()
                      []
                      tyBool
                      (ELet () (PVar () "a") (bool True) (var "a"))
                  )
                  (var "f")
              ),
              ("let a = 100; a", ELet () (PVar () "a") (int 100) (var "a")),
              ( "let (a,b) = (1,2); a + b",
                ELet
                  ()
                  (PTuple () (PVar () "a") (NE.singleton $ PVar () "b"))
                  (tuple [int 1, int 2])
                  (EInfix () OpAdd (var "a") (var "b"))
              ),
              ( "let a : Int64 = 1; True",
                ELet () (PVar () "a") (EAnn () tyInt64 (int 1)) (bool True)
              ),
              ("dogs(); 100", ELet () (PWildcard ()) (EApply () "dogs" []) (int 100)),
              ("100; 100", ELet () (PWildcard ()) (int 100) (int 100)),
              ("(100 : Int32)", EAnn () tyInt32 (int 100)),
              ("load(100)", ELoad () (int 100)),
              ("store(100, (200 : Int64))", EStore () (int 100) (EAnn () tyInt64 (int 200))),
              ( "if True then { let a = True; False } else False",
                EIf
                  ()
                  (bool True)
                  (EBlock () (ELet () (PVar () "a") (bool True) (bool False)))
                  (bool False)
              ),
              ("Red", EConstructor () "Red" []),
              ("Nothing ", EConstructor () "Nothing" []),
              ("Some(1)", EConstructor () "Some" [int 1]),
              ("These(True,False)", EConstructor () "These" [bool True, bool False]),
              ( "case a { (1,2) -> 0, (a,b) -> a + b }",
                EMatch
                  ()
                  (var "a")
                  ( NE.fromList
                      [ ( patTuple [patInt 1, patInt 2],
                          int 0
                        ),
                        ( patTuple [patVar "a", patVar "b"],
                          EInfix () OpAdd (var "a") (var "b")
                        )
                      ]
                  )
              ),
              ( "case a { 1 -> 0, other -> other, }",
                EMatch
                  ()
                  (var "a")
                  ( NE.fromList
                      [ ( patInt 1,
                          int 0
                        ),
                        ( patVar "other",
                          var "other"
                        )
                      ]
                  )
              )
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
