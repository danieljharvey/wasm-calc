{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc.Module (resolveModule) where

import Calc.Types.Memory
import Calc.Types.Module
import Control.Monad (foldM)
import Control.Monad.Except

data ResolveModuleError ann = MultipleMemoryItems ann ann
  deriving stock (Eq, Ord, Show, Functor)

-- | turn lots of parts into a module
resolveModule ::
  (MonadError (ResolveModuleError ann) m) =>
  [ModuleItem ann] ->
  m (Module ann)
resolveModule parts =
  let emptyModule =
        Module
          { mdFunctions = mempty,
            mdImports = mempty,
            mdMemory = Nothing,
            mdGlobals = mempty,
            mdTests = mempty,
            mdDataTypes = mempty
          }
      withPart newModule item = case item of
        ModuleFunction func ->
          pure $
            newModule {mdFunctions = mdFunctions newModule <> [func]}
        ModuleImport imp ->
          pure $
            newModule {mdImports = mdImports newModule <> [imp]}
        ModuleGlobal glob ->
          pure $
            newModule {mdGlobals = mdGlobals newModule <> [glob]}
        ModuleTest test ->
          pure $
            newModule {mdTests = mdTests newModule <> [test]}
        ModuleData dt ->
          pure $
            newModule {mdDataTypes = mdDataTypes newModule <> [dt]}
        ModuleMemory mem -> do
          case mdMemory newModule of
            Nothing -> pure $ newModule {mdMemory = Just mem}
            Just oldMem ->
              throwError (MultipleMemoryItems (imAnn oldMem) (imAnn mem))
        ModuleNested _ _ -> error "resolveModule nested"
   in foldM withPart emptyModule parts
