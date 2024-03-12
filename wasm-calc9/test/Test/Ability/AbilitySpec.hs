{-# LANGUAGE OverloadedStrings #-}

module Test.Ability.AbilitySpec (spec) where

import Calc
import Calc.Ability.Check
import Calc.Types.Ability
import Control.Monad (void)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  fdescribe "AbilitySpec" $ do
    describe "getExprAbilities" $ do
      let strings =
            [ ( "1 + 2",
                mempty
              ),
              ( "set(myGlobal, 1)",
                S.singleton (MutateGlobal () "myGlobal")
              ),
              ( "(1,2,3)",
                S.singleton (AllocateMemory ())
              )
            ]
      traverse_
        ( \(str, abilityResult) -> it (T.unpack str) $ do
            case parseExprAndFormatError str of
              Right parsedExpr -> do
                S.map void (abilityCheckExpr (AbilityEnv mempty) parsedExpr) `shouldBe` abilityResult
              Left e -> error (T.unpack e)
        )
        strings

    describe "getModuleAbilities" $ do
      let strings =
            [ ( "import console.log as consoleLog(number: Int64) -> Void",
                ModuleAbilities {maFunctions = mempty}
              ),
              ( "function main() -> Int32 { 1 }",
                ModuleAbilities {maFunctions = M.singleton "main" mempty}
              ),
              ( joinLines
                  [ "import console.log as consoleLog(number: Int64) -> Void",
                    "function main() -> Void { consoleLog(100) }"
                  ],
                ModuleAbilities {maFunctions = M.singleton "main" (S.singleton $ CallImportedFunction () "consoleLog")}
              ),
              ( joinLines
                  [ "import console.log as consoleLog(number: Int64) -> Void",
                    "function log() -> Void { consoleLog(100) }",
                    "function main() -> Void { log() }"
                  ],
                ModuleAbilities
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
                let moduleAbilities = abilityCheckModule parsedModule
                moduleAbilities {maFunctions = fmap (S.map void) (maFunctions moduleAbilities)} `shouldBe` abilityResult
              Left e -> error (T.unpack e)
        )
        strings
