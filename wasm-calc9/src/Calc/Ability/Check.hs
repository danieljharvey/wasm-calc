{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Ability.Check (AbilityEnv (..), ModuleAbilities (..), abilityCheckModule, abilityCheckExpr) where

import Calc.ExprUtils
import Calc.Types.Ability
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Import
import Calc.Types.Module
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data ModuleAbilities ann = ModuleAbilities
  { maFunctions :: M.Map Identifier (S.Set (Ability ann))
  }
  deriving stock (Eq, Ord, Show)

data AbilityEnv = AbilityEnv
  { -- | which functions are in fact imports?
    aeImportNames :: S.Set FunctionName
  }

-- | simplified typechecking, we find out which abilities each function uses
abilityCheckExpr :: (Ord ann) => AbilityEnv -> Expr ann -> S.Set (Ability ann)
abilityCheckExpr env = snd . runWriter . flip runReaderT env . abilityExpr

abilityCheckModule :: (Ord ann) => Module ann -> ModuleAbilities ann
abilityCheckModule (Module {mdImports, mdFunctions}) =
  let importNames = S.fromList $ (\(Import {impImportName}) -> impImportName) <$> mdImports

      abilityEnv = AbilityEnv {aeImportNames = importNames}

      checkFunction (Function {fnFunctionName = FunctionName ident, fnBody}) =
        (Identifier ident, abilityCheckExpr abilityEnv fnBody)
   in ModuleAbilities
        { maFunctions = M.fromList (checkFunction <$> mdFunctions)
        }

abilityExpr ::
  ( MonadReader AbilityEnv m,
    MonadWriter (S.Set (Ability ann)) m
  ) =>
  Expr ann ->
  m (Expr ann)
abilityExpr (ESet ann ident value) = do
  tell (S.singleton $ MutateGlobal ann ident)
  pure (ESet ann ident value)
abilityExpr (ETuple ann a b) = do
  -- we'll need to account for other allocations in future
  tell (S.singleton $ AllocateMemory ann)
  pure (ETuple ann a b)
abilityExpr (EApply ann fn args) = do
  isImport <- asks (S.member fn . aeImportNames)
  when isImport $
    tell (S.singleton $ CallImportedFunction ann fn)
  pure (EApply ann fn args)
abilityExpr other = bindExpr abilityExpr other
