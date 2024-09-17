{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Typecheck.Types
  ( TypecheckM (..),
    TypecheckState (..),
    TypecheckEnv (..),
    TypecheckGlobal (..),
    TypeScheme (..),
    TCDataType (..),
  )
where

import Calc.Typecheck.Error
import Calc.Types
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Natural

-- | data type information for a single constructor
data TCDataType ann = TCDataType
  { tcdtName :: DataName,
    tcdtGenerics :: [TypeVar],
    tcdtArgs :: [Type ann]
  }
  deriving stock (Eq, Ord, Show)

-- | temporary read-only state
data TypecheckEnv ann = TypecheckEnv
  { tceVars :: M.Map Identifier (Type ann),
    tceGenerics :: S.Set TypeVar,
    tceMemoryLimit :: Natural,
    tceDataTypes :: M.Map Constructor (TCDataType ann)
  }
  deriving stock (Eq, Ord, Show)

data TypecheckGlobal ann = TypecheckGlobal
  { tcgType :: Type ann,
    tcgMutable :: Mutability
  }
  deriving stock (Eq, Ord, Show)

data TypecheckState ann = TypecheckState
  { tcsFunctions :: M.Map (WithPath FunctionName) (TypeScheme ann),
    tcsGlobals :: M.Map Identifier (TypecheckGlobal ann),
    tcsUnique :: Natural,
    tcsUnified :: M.Map Natural (Type ann)
  }
  deriving stock (Eq, Ord, Show)

data TypeScheme ann = TypeScheme
  { tsType :: Type ann,
    tsGenerics :: S.Set TypeVar
  }
  deriving stock (Eq, Ord, Show)

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
