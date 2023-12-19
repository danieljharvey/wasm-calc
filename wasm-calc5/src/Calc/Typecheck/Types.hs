{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Typecheck.Types
  ( TypecheckM (..),
    runTypecheckM,
    TypecheckEnv (..),
    lookupVar,
    withVar,
    lookupFunction,
    withFunctionEnv,
    storeFunction,
  )
where

import           Calc.Typecheck.Error
import           Calc.Types.Function
import           Calc.Types.Identifier
import           Calc.Types.Type
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor        (first)
import qualified Data.HashMap.Strict   as HM
import qualified Data.Set              as S

-- | temporary read-only state
data TypecheckEnv ann = TypecheckEnv
  { tceVars     :: HM.HashMap Identifier (Type ann),
    tceGenerics :: S.Set TypeVar
  }
  deriving stock (Eq, Ord, Show)

newtype TypecheckState ann = TypecheckState
  {tcsFunctions :: HM.HashMap FunctionName (TypeScheme ann)}
  deriving stock (Eq, Ord, Show)

data TypeScheme ann
  = TypeScheme { tsType :: Type ann, tsGenerics :: S.Set TypeVar }
  deriving stock (Eq,Ord,Show)

newtype TypecheckM ann a = TypecheckM
  { getTypecheckM ::
      ReaderT (TypecheckEnv ann) (StateT (TypecheckState ann) (Either (TypeError ann))) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (TypecheckEnv ann),
      MonadError (TypeError ann),
      MonadState (TypecheckState ann)
    )

runTypecheckM ::
  TypecheckEnv ann ->
  TypecheckM ann a ->
  Either (TypeError ann) a
runTypecheckM env action =
  evalStateT (runReaderT (getTypecheckM action) env) (TypecheckState mempty)

storeFunction ::
  FunctionName ->
  S.Set TypeVar ->
  Type ann ->
  TypecheckM ann ()
storeFunction fnName generics ty =
  modify
    ( \tcs ->
        tcs
          { tcsFunctions =
              HM.insert fnName (TypeScheme ty generics) (tcsFunctions tcs)
          }
    )

-- | look up a saved identifier "in the environment"
lookupFunction :: ann -> FunctionName -> TypecheckM ann (Type ann)
lookupFunction ann fnName = do
  maybeType <- gets (HM.lookup fnName . tcsFunctions)
  case maybeType of
    Just found -> pure (tsType found ) -- TODO: generalise this type with type scheme
    Nothing -> do
      allFunctions <- gets (HM.keysSet . tcsFunctions)
      throwError (FunctionNotFound ann fnName allFunctions)

-- | look up a saved identifier "in the environment"
lookupVar :: ann -> Identifier -> TypecheckM ann (Type ann)
lookupVar ann identifier = do
  maybeType <- asks (HM.lookup identifier . tceVars)
  case maybeType of
    Just found -> pure found
    Nothing -> do
      allIdentifiers <- asks (HM.keysSet . tceVars)
      throwError (VarNotFound ann identifier allIdentifiers)

-- | add an identifier to the environment
withVar :: Identifier -> Type ann -> TypecheckM ann a -> TypecheckM ann a
withVar identifier ty =
  local
    ( \tce ->
        tce
          { tceVars =
              HM.insert identifier ty (tceVars tce)
          }
    )

-- | temporarily add function arguments and generics into the Reader env
withFunctionEnv ::
  [(ArgumentName, Type ann)] ->
  S.Set TypeVar ->
  TypecheckM ann a ->
  TypecheckM ann a
withFunctionEnv args generics =
  let identifiers = fmap (first (\(ArgumentName arg) -> Identifier arg)) args
   in local
    ( \tce ->
        tce
          { tceVars = tceVars tce <> HM.fromList identifiers,
            tceGenerics = generics
          }
    )
