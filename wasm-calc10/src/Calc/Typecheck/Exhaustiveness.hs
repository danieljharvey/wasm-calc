module Calc.Typecheck.Exhaustiveness (generateFromPattern) where

import Data.Functor (void)
import Calc.Types.Type
import Calc.Types.Pattern
import Calc.Types.Prim
import qualified Data.Set as S

generateFromType :: Type ann -> S.Set (Pattern ())
generateFromType (TPrim _ TBool) = S.fromList [PLiteral () (PBool True),PLiteral () (PBool False)]
generateFromType (TPrim _ _) = S.singleton (PWildcard ())
generateFromType (TContainer _ (a : b : bs)) =
  -- this here is your fucking stinker, friend
  generateFromType a <> generateFromType b <> foldMap generateFromType bs
generateFromType _ = mempty

generateFromPattern :: Pattern (Type ann) -> S.Set (Pattern ())
generateFromPattern pat@(PLiteral ty _ ) =
  S.difference (generateFromType ty) (S.singleton (void pat))
generateFromPattern pat@(PTuple ty _a _as) =
  S.difference (generateFromType ty) (S.singleton (void pat))
generateFromPattern _ = mempty
