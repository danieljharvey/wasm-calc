{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Calc.Ability.Check (AbilityEnv (..), ModuleAbilities (..), AbilityError(..), abilityCheckModule) where

import           Calc.ExprUtils
import           Calc.Types.Ability
import           Calc.Types.Expr
import           Calc.Types.Function
import           Calc.Types.Identifier
import           Calc.Types.Import
import           Calc.Types.Module
import           Calc.Types.Test
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Foldable          (traverse_)
import qualified Data.List              as List
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S

data ModuleAbilities ann = ModuleAbilities
  { maFunctions :: M.Map FunctionName (S.Set (Ability ann)),
    maTests     :: M.Map Identifier (S.Set (Ability ann))
  }
  deriving stock (Eq, Ord, Show)

data AbilityEnv = AbilityEnv
  { -- | which functions are in fact imports?
    aeImportNames :: S.Set FunctionName
  }

data AbilityError ann
  = FunctionAbilityViolatesConstraint { aeConstraint :: String, aeAbility :: Ability ann, aeFunctionName :: FunctionName }
  | TestViolatesConstraint { aeAbility :: Ability ann, aeTestName :: Identifier }
  deriving stock (Eq,Ord,Show,Functor)

newtype AbilityM ann a = AbilityM (StateT (ModuleAbilities ann) (ReaderT AbilityEnv (Writer (S.Set (Ability ann)))) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (ModuleAbilities ann),
      MonadReader AbilityEnv,
      MonadWriter (S.Set (Ability ann))
    )

abilityCheckModule :: (Ord ann) => Module ann -> Either (AbilityError ann) (ModuleAbilities ann)
abilityCheckModule theModule = do
  let moduleAbilities = getAbilitiesForModule theModule

      checkTest (testName, abilities) =
          case List.find (\case
             CallImportedFunction {} -> True
             _ -> False) (S.toList abilities) of
                       Just violatingAbility -> Left (TestViolatesConstraint violatingAbility testName)
                       Nothing -> Right ()

  traverse_ checkTest (M.toList $ maTests moduleAbilities)
  pure moduleAbilities

getAbilitiesForModule :: (Ord ann) => Module ann -> ModuleAbilities ann
getAbilitiesForModule (Module {mdImports, mdFunctions, mdTests}) =
  let importNames = S.fromList $ (\(Import {impImportName}) -> impImportName) <$> mdImports

      abilityEnv = AbilityEnv {aeImportNames = importNames}

      initialState = ModuleAbilities mempty mempty

      getAbilitiesForFunction (Function {fnFunctionName, fnBody}) = do
        functionAbilities <- execWriterT (abilityExpr fnBody)
        modify
          ( \ma ->
              ma
                { maFunctions =
                    M.insert fnFunctionName functionAbilities (maFunctions ma)
                }
          )

      getAbilitiesForTests (Test {tesName,tesExpr}) = do
        testAbilities <- execWriterT (abilityExpr tesExpr)
        modify
          ( \ma ->
              ma
                { maTests =
                    M.insert tesName testAbilities (maTests ma)
                }
          )

      action = do
        traverse_ getAbilitiesForFunction mdFunctions
        traverse_ getAbilitiesForTests mdTests

   in runAbilityM initialState abilityEnv action

-- | get the abilities out of our pile o' monads
runAbilityM :: (Ord ann) => ModuleAbilities ann -> AbilityEnv -> AbilityM ann a -> ModuleAbilities ann
runAbilityM moduleAbilities abilityEnv (AbilityM action) =
  fst $ runIdentity $ runWriterT $ runReaderT (execStateT action moduleAbilities) abilityEnv

lookupFunctionAbilities ::
  (Ord ann) =>
  (MonadState (ModuleAbilities ann) m) =>
  FunctionName ->
  m (S.Set (Ability ann))
lookupFunctionAbilities fnName = do
  functionAbilities <- gets (M.lookup fnName . maFunctions)
  case functionAbilities of
    Just abilities -> pure abilities
    Nothing        -> pure mempty

abilityExpr ::
  ( MonadState (ModuleAbilities ann) m,
    MonadReader AbilityEnv m,
    MonadWriter (S.Set (Ability ann)) m,
    Ord ann
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
  if isImport
    then tell (S.singleton $ CallImportedFunction ann fn)
    else do
      -- whatever abilities this function uses, we now use
      functionAbilities <- lookupFunctionAbilities fn
      tell functionAbilities
  pure (EApply ann fn args)
abilityExpr other = bindExpr abilityExpr other
