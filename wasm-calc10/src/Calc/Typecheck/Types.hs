{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Calc.Typecheck.Types
  ( TypecheckM (..),
    TypecheckState (..),
    TypecheckEnv (..),
    TypecheckGlobal (..),
    TypeScheme (..)
  )
where

import Calc.Typecheck.Error
import Calc.Types
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import GHC.Natural

-- | temporary read-only state
data TypecheckEnv ann = TypecheckEnv
  { tceVars :: HM.HashMap Identifier (Type ann),
    tceGenerics :: S.Set TypeVar,
    tceMemoryLimit :: Natural
  }
  deriving stock (Eq, Ord, Show)

data TypecheckGlobal ann = TypecheckGlobal
  { tcgType :: Type ann,
    tcgMutable :: Mutability
  }
  deriving stock (Eq, Ord, Show)

data TypecheckState ann = TypecheckState
  { tcsFunctions :: HM.HashMap FunctionName (TypeScheme ann),
    tcsGlobals :: HM.HashMap Identifier (TypecheckGlobal ann),
    tcsUnique :: Natural,
    tcsUnified :: HM.HashMap Natural (Type ann)
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
