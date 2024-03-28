{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Ability.AbilitySpec (spec) where

import           Calc
import           Calc.Ability.Check
import           Calc.Types.Ability
import           Control.Monad      (void)
import           Data.Foldable      (traverse_)
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import qualified Data.Text          as T
import           Test.Helpers
import           Test.Hspec

voidModuleAbilities :: ModuleAbilities ann -> ModuleAbilities ()
voidModuleAbilities moduleAbilities =
  moduleAbilities
    { maFunctions = fmap (S.map void) (maFunctions moduleAbilities),
      maTests = fmap (S.map void) (maTests moduleAbilities)
    }

fromLeft :: (Show a) => Either e a -> e
fromLeft = \case
  Left e -> e
  Right a -> error (show a)

fromRight :: (Show e) => Either e a -> a
fromRight = \case
  Right a -> a
  Left e -> error (show e)

spec :: Spec
spec = do
  describe "AbilitySpec" $
    describe "abilityCheckModule" $ do
      describe "Success" $ do
        let emptyModuleAbilities =
              ModuleAnnotations
                { maFunctions = mempty,
                  maTests = mempty
                }
        let successes =
              [ ( "function main() -> Void { set(myGlobal, 1) }",
                  emptyModuleAbilities {maFunctions = M.singleton "main" (S.singleton (MutateGlobal () "myGlobal"))}
                ),
                ( "function main() -> (Int32, Int32, Int32) { (1,2,3) }",
                  emptyModuleAbilities {maFunctions = M.singleton "main" (S.singleton (AllocateMemory ()))}
                ),
                ( "test horse = { (1,2,3) }",
                  emptyModuleAbilities {maTests = M.singleton "horse" (S.singleton (AllocateMemory ()))}
                ),
                ( "import console.log as consoleLog(number: Int64) -> Void",
                  emptyModuleAbilities {maFunctions = mempty}
                ),
                ( "function main() -> Int32 { 1 }",
                  emptyModuleAbilities {maFunctions = M.singleton "main" mempty}
                ),
                ( joinLines
                    [ "import console.log as consoleLog(number: Int64) -> Void",
                      "function main() -> Void { consoleLog(100) }"
                    ],
                  emptyModuleAbilities {maFunctions = M.singleton "main" (S.singleton $ CallImportedFunction () "consoleLog")}
                ),
                ( joinLines
                    [ "import console.log as consoleLog(number: Int64) -> Void",
                      "function log() -> Void { consoleLog(100) }",
                      "function main() -> Void { log() }"
                    ],
                  emptyModuleAbilities
                    { maFunctions =
                        M.fromList
                          [ ("main", S.singleton $ CallImportedFunction () "consoleLog"),
                            ("log", S.singleton $ CallImportedFunction () "consoleLog")
                          ]
                    }
                )
              ]
        traverse_
          ( \(str, abilityResult) -> it (T.unpack str) $ do
              case parseModuleAndFormatError str of
                Right parsedModule -> do
                  voidModuleAbilities (fromRight (abilityCheckModule parsedModule))
                    `shouldBe` abilityResult
                Left e -> error (T.unpack e)
          )
          successes

      describe "Failures" $ do
        let failures =
              [ ( joinLines
                    [ "import console.log as consoleLog(number: Int64) -> Void",
                      "test horse = { consoleLog(100) }"
                    ],
                  TestViolatesConstraint {aeTestName = "horse", aeAbility = CallImportedFunction () "consoleLog"}
                ),
                ( joinLines
                    [
                      "function [noglobalmutate] mutate() -> Void { set(counter,1) }"
                    ],
                  FunctionViolatesConstraint {aeFunctionName = "mutate",
                      aeConstraint = NoGlobalMutate,
                      aeAbility = MutateGlobal () "counter"}
                ),
                ( joinLines
                    [
                      "function [noallocate] allocate() -> Void { (1,1,1) }"
                    ],
                  FunctionViolatesConstraint {aeFunctionName = "allocate",
                      aeConstraint = NoAllocate,
                      aeAbility = AllocateMemory () }
                )



              ]
        traverse_
          ( \(str, abilityError) -> it (T.unpack str) $ do
              case parseModuleAndFormatError str of
                Right parsedModule -> do
                  void (fromLeft (abilityCheckModule parsedModule))
                    `shouldBe` abilityError
                Left e -> error (T.unpack e)
          )
          failures
