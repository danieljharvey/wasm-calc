module Calc.Typecheck.Generalise (generalise) where

import Calc.TypeUtils (bindType)
import Calc.Typecheck.Types
import Calc.Types.Type
import Calc.Types.TypeVar
import Control.Monad.State
import qualified Data.Set as S
import GHC.Natural

-- get a nice new number
freshUnificationVariable :: TypecheckM ann Natural
freshUnificationVariable = do
  current <- gets tcsUnique
  modify (\tcs -> tcs {tcsUnique = current + 1})
  pure current

-- given a type, replace anything that should be generic with unification
-- variables so that we know to replace them with types easily
generalise :: S.Set TypeVar -> Type ann -> TypecheckM ann (Type ann)
generalise generics (TVar ann var)
  | S.member var generics =
      TUnificationVar ann <$> freshUnificationVariable
generalise generics other = bindType (generalise generics) other
