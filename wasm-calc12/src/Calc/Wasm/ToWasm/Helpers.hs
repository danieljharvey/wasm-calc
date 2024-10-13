{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.ToWasm.Helpers
  ( UsesAllocator (..),
    moduleUsesAllocator,
    allocCountIndex,
    testName,
    allocIndex,
    dropIndex,
    functionOffset,
    globalOffset,
    generatedFunctionOffset,
  )
where

import Calc.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad.Reader
import Data.Bool (bool)
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Natural

data UsesAllocator = UsesAllocator | DoesNotUseAllocator
  deriving stock (Eq, Ord, Show)

moduleUsesAllocator :: WasmModule -> UsesAllocator
moduleUsesAllocator =
  bool DoesNotUseAllocator UsesAllocator
    . getAny
    . foldMap (Any . S.member (AllocateMemory ()) . wfAbilities)
    . wmFunctions

allocIndex :: (MonadReader ToWasmEnv m) => m Natural
allocIndex = asks tweImportsOffset

dropIndex :: (MonadReader ToWasmEnv m) => m Natural
dropIndex = asks ((+ 1) . tweImportsOffset)

allocCountIndex :: (MonadReader ToWasmEnv m) => m Natural
allocCountIndex = asks ((+ 2) . tweImportsOffset)

functionOffset :: ToWasmEnv -> Natural
functionOffset
  twe = tweImportsOffset twe + tweFunctionsOffset twe

generatedFunctionOffset :: ToWasmEnv -> Natural
generatedFunctionOffset
  twe =
    tweImportsOffset twe
      + tweFunctionsOffset twe
      + tweGeneratedFunctionOffset twe

globalOffset :: (MonadReader ToWasmEnv m) => m Natural
globalOffset = asks tweGlobalOffset

-- what do we call tests in Wasm exports
testName :: WasmTest -> T.Text
testName (WasmTest {wtName}) = "_test_" <> wtName
