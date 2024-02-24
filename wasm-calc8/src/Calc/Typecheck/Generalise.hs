module Calc.Typecheck.Generalise (freshUnificationVariable, generalise) where

import Calc.TypeUtils (mapType)
import Calc.Typecheck.Types
import Calc.Types.Type
import Calc.Types.TypeVar
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import GHC.Natural

-- get a nice new number
freshUnificationVariable :: TypecheckM ann Natural
freshUnificationVariable = do
  modify (\tcs -> tcs {tcsUnique = tcsUnique tcs + 1})
  gets tcsUnique

allFresh :: S.Set TypeVar -> TypecheckM ann (HM.HashMap TypeVar Natural)
allFresh generics =
  let freshOne typeVar =
        HM.singleton typeVar <$> freshUnificationVariable
   in mconcat <$> traverse freshOne (S.toList generics)

-- given a type, replace anything that should be generic with unification
-- variables so that we know to replace them with types easily
generalise :: S.Set TypeVar -> Type ann -> TypecheckM ann (Type ann)
generalise generics ty =
  do
    fresh <- allFresh generics
    pure $ generaliseInternal fresh ty

-- given a type, replace anything that should be generic with unification
-- variables so that we know to replace them with types easily
generaliseInternal :: HM.HashMap TypeVar Natural -> Type ann -> Type ann
generaliseInternal fresh (TVar ann var) =
  case HM.lookup var fresh of
    Just nat ->
      TUnificationVar ann nat
    Nothing -> error "oh no generalise error"
generaliseInternal fresh other =
  mapType (generaliseInternal fresh) other
