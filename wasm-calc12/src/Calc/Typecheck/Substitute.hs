module Calc.Typecheck.Substitute (substitute) where

import Calc.TypeUtils
import Calc.Types.Type
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import GHC.Natural

substitute ::
  M.Map Natural (Type ann) ->
  Type ann ->
  Type ann
substitute subs oldTy@(TUnificationVar _ nat) =
  fromMaybe oldTy (M.lookup nat subs)
substitute subs other =
  mapType (substitute subs) other
