{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Calc.Module (resolveModules ) where

import Control.Monad.Writer
import qualified Data.Map as M
import Calc.Types.Memory
import Calc.Types.Module
import Control.Monad (foldM)
import Control.Monad.Except
import Calc.Types.WithPath

data ResolveModuleError ann = MultipleMemoryItems ann ann
  deriving stock (Eq, Ord, Show, Functor)

resolveModules :: (MonadError (ResolveModuleError ann) m) =>
  [ModuleItem ann] -> m (M.Map ModulePath (Module ann))
resolveModules parts = do
  (mainModule,otherModules) <- runWriterT (resolveModule (ModulePath []) parts)

  pure (M.insert (ModulePath []) mainModule otherModules)

-- | turn lots of parts into a module
resolveModule ::
  (MonadError (ResolveModuleError ann) m, MonadWriter (M.Map ModulePath (Module ann)) m) =>
  ModulePath ->
  [ModuleItem ann] ->
  m (Module ann)
resolveModule (ModulePath modulePath) parts =
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
        ModuleNested moduleName innerParts -> do
          let innerPath = ModulePath $ moduleName : modulePath
          innerModule <- resolveModule innerPath innerParts
          tell $ M.singleton innerPath innerModule
          pure $ newModule

   in foldM withPart emptyModule parts
