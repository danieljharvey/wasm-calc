module Calc.Typecheck.Substitute (substitute) where

import Calc.TypeUtils
import Calc.Types.Type
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import GHC.Natural

substitute ::
  HM.HashMap Natural (Type ann) ->
  Type ann ->
  Type ann
substitute subs oldTy@(TUnificationVar _ nat) =
  fromMaybe oldTy (HM.lookup nat subs)
substitute subs other =
  mapType (substitute subs) other
