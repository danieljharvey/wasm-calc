module Calc.Typecheck.Substitute (substitute,substituteOrFail) where

import Calc.TypeUtils
import Calc.Types.Type
import qualified Data.HashMap.Strict as HM
import GHC.Natural
import Calc.Typecheck.Types
import Calc.Typecheck.Error
import Control.Monad.Except

substitute ::
  HM.HashMap Natural (TCUnified ann) ->
  Type ann ->
  Type ann
substitute subs oldTy@(TUnificationVar _ nat) =
  case HM.lookup nat subs of
    Just (IsKnown ty) -> ty
    _ -> oldTy
substitute subs other =
  mapType (substitute subs) other

-- | a version of substitute where we fail if we don't resolve an int or float
-- literal
substituteOrFail ::
  HM.HashMap Natural (TCUnified ann) ->
  Type ann ->
  TypecheckM ann (Type ann)
substituteOrFail subs oldTy@(TUnificationVar ann nat) =
  case HM.lookup nat subs of
    Just (IsKnown ty) -> pure ty
    Just IsIntLit -> throwError (UnknownIntegerLiteral ann)
    Just IsFloatLit -> throwError (UnknownFloatLiteral ann)
    _ -> pure oldTy
substituteOrFail subs other =
  bindType (substituteOrFail subs) other
