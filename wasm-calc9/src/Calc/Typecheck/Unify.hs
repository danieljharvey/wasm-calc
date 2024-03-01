module Calc.Typecheck.Unify
  ( unify,
  )
where

import Calc.Typecheck.Error
import Calc.Typecheck.Helpers
import Calc.Typecheck.Types
import Calc.Types.Type
import Control.Monad (zipWithM)
import Control.Monad.Except
import Data.Functor
import qualified Data.List.NonEmpty as NE

-- unification. for our simple purposes this means "smash two types
-- together and see what we learn" (or explode if it makes no sense)
unify :: Type ann -> Type ann -> TypecheckM ann (Type ann)
unify (TUnificationVar _ nat) b = do
  unifyVariableWithType nat b
unify a (TUnificationVar _ nat) = do
  unifyVariableWithType nat a
unify (TFunction ann argA bodyA) (TFunction _ argB bodyB) =
  TFunction ann
    <$> zipWithM unify argA argB
    <*> unify bodyA bodyB
unify (TContainer ann as) (TContainer _ bs) =
  TContainer ann
    <$> ( NE.fromList
            <$> zipWithM unify (NE.toList as) (NE.toList bs)
        )
unify tyA tyB =
  if void tyA == void tyB
    then pure tyA
    else throwError (TypeMismatch tyA tyB)
