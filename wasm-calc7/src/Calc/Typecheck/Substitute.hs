module Calc.Typecheck.Substitute (substitute) where

import Calc.TypeUtils
import Calc.Types.Type
import qualified Data.HashMap.Strict as HM
import GHC.Natural

substitute ::
  HM.HashMap Natural (Type ann) ->
  Type ann ->
  Type ann
substitute subs (TUnificationVar _ nat) =
  case HM.lookup nat subs of
    Just ty -> ty
    Nothing -> error $ "Could not find unification var for " <> show nat
substitute subs other =
  mapType (substitute subs) other
