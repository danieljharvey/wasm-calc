{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Dependencies (Dependency (..), treeShakeModule, trimDependencies, combineDependencies, getModuleDependencies) where

import Calc.ExprUtils
import Calc.Types
import Calc.Types.ModuleAnnotations
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

data Dependency
  = DepFunction FunctionName
  | DepTest Identifier
  | DepImport FunctionName
  deriving stock (Eq, Ord, Show)

-- | remove anything that is not used by an exported function
-- | todo, remove unrequired globals
treeShakeModule :: Module ann -> Module ann
treeShakeModule wholeModule =
  let annotatedModule = getModuleDependencies wholeModule
      exportedFunctionNames =
        S.fromList $ mapMaybe (\Function {fnFunctionName, fnPublic} -> if fnPublic then Just (DepFunction fnFunctionName) else Nothing) (mdFunctions wholeModule)
   in trimDependencies exportedFunctionNames annotatedModule wholeModule

-- | given a set of required dependencies, strip out everything that isn't
-- needed by them
trimDependencies :: S.Set Dependency -> ModuleAnnotations (S.Set Dependency) -> Module ann -> Module ann
trimDependencies deps moduleAnnotations wholeMod =
  let requiredDependencies = combineDependencies deps moduleAnnotations
      filteredFunctions =
        filter
          ( \Function {fnFunctionName} ->
              S.member (DepFunction fnFunctionName) requiredDependencies
          )
          (mdFunctions wholeMod)
      filteredTests = filter (\Test {tesName} -> S.member (DepTest tesName) requiredDependencies) (mdTests wholeMod)
      filteredImports = filter (\Import {impImportName} -> S.member (DepImport impImportName) requiredDependencies) (mdImports wholeMod)
   in wholeMod {mdFunctions = filteredFunctions, mdTests = filteredTests, mdImports = filteredImports}

-- | recursively look through ModuleAnnotations to get complete set of
-- dependencies
combineDependencies :: S.Set Dependency -> ModuleAnnotations (S.Set Dependency) -> S.Set Dependency
combineDependencies deps _ | S.null deps = mempty
combineDependencies deps annotatedModule =
  let getChildDeps (DepFunction fnName) = case M.lookup fnName (maFunctions annotatedModule) of
        Just functionDeps -> functionDeps
        Nothing -> error $ "Internal error looking up " <> show fnName
      getChildDeps (DepTest identifier) = case M.lookup identifier (maTests annotatedModule) of
        Just testDeps -> testDeps
        Nothing -> error $ "Internal error looking up " <> show identifier
      getChildDeps (DepImport _) = mempty

      childDeps = foldMap getChildDeps deps
      newDeps = S.difference childDeps deps -- what new dependencies have we uncovered?
   in deps <> childDeps <> combineDependencies newDeps annotatedModule

getModuleDependencies :: Module ann -> ModuleAnnotations (S.Set Dependency)
getModuleDependencies (Module {mdTests, mdFunctions, mdImports}) =
  let importNames = S.fromList $ (\Import {impImportName} -> impImportName) <$> mdImports
      maTests = M.fromList $ getTestDependencies <$> mdTests
      maFunctions = M.fromList $ getFunctionDependencies importNames <$> mdFunctions
   in ModuleAnnotations {maFunctions, maTests}

getTestDependencies :: Test ann -> (Identifier, S.Set Dependency)
getTestDependencies (Test {tesName, tesExpr}) =
  (tesName, getExprDependencies mempty tesExpr)

getFunctionDependencies :: S.Set FunctionName -> Function ann -> (FunctionName, S.Set Dependency)
getFunctionDependencies importNames (Function {fnFunctionName, fnBody}) =
  (fnFunctionName, getExprDependencies importNames fnBody)

getExprDependencies :: S.Set FunctionName -> Expr ann -> S.Set Dependency
getExprDependencies importNames = snd . runWriter . go
  where
    go (EApply ann fnName args) = do
      if S.member fnName importNames
        then tell (S.singleton (DepImport fnName))
        else tell (S.singleton (DepFunction fnName))
      EApply ann fnName <$> traverse go args
    go other = bindExpr go other
