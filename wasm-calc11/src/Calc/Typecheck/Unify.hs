module Calc.Typecheck.Unify
  ( unify,
  )
where

import Calc.Typecheck.Error
import Calc.Typecheck.Types
import Calc.Types.Type
import Control.Monad (zipWithM)
import Control.Monad.Except
import Control.Monad.State
import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import GHC.Natural

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
unify (TConstructor ann dataNameA argsA) (TConstructor _ dataNameB argsB)
  | dataNameA == dataNameB && length argsA == length argsB =
      TConstructor ann dataNameA <$> zipWithM unify argsA argsB
unify tyA tyB =
  if void tyA == void tyB
    then pure tyA
    else throwError (TypeMismatch tyA tyB)

storeUnified :: Natural -> Type ann -> TypecheckM ann ()
storeUnified nat ty =
  modify
    ( \tcs ->
        tcs
          { tcsUnified =
              HM.insert nat (IsKnown ty) (tcsUnified tcs)
          }
    )

-- | given a unification variable, either save it and return the type
-- or explode because we've already unified it with something else
unifyVariableWithType ::
  Natural ->
  Type ann ->
  TypecheckM ann (Type ann)
unifyVariableWithType nat ty =
  do
    existing <- gets (HM.lookup nat . tcsUnified)
    case existing of
      Nothing -> do
        -- this is the first match, store it and return the passed-in type
        storeUnified nat ty
        pure ty
      Just (IsKnown existingTy) -> do
        unify existingTy ty
      Just IsIntLit -> error "is it an int lit though"
      Just IsFloatLit -> error "is it a float lit though"
