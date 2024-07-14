{-# LANGUAGE FlexibleContexts #-}

module Calc.Typecheck.Patterns.Generate
  ( generate,
  )
where

import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import Data.Functor (void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

generateFromType :: Type ann -> S.Set (Pattern ())
generateFromType (TPrim _ TBool) = S.fromList [PLiteral () (PBool True), PLiteral () (PBool False)]
generateFromType (TPrim _ _) = S.singleton (PWildcard ())
generateFromType _ = mempty

-- generateFromPattern, except if we get nothing, we return ourselves,
-- to stop cartesian product being eliminated by empty value
safeGenerateFromPattern :: Pattern (Type ann) -> S.Set (Pattern ())
safeGenerateFromPattern pat =
  let pats = generateFromPattern pat
   in if S.null pats then S.singleton (void pat) else pats

generateFromPattern :: Pattern (Type ann) -> S.Set (Pattern ())
generateFromPattern (PLiteral ty _) =
  generateFromType ty
generateFromPattern (PTuple _ty patA patAs) =
  let pats = patA : NE.toList patAs
      manyRows = traverse (fmap void . S.toList . safeGenerateFromPattern) pats
      fromRow patList =
        case patList of
          (a : b : bs) -> S.singleton $ PTuple () a (b NE.:| bs)
          other -> error (show other)
   in foldMap fromRow manyRows
generateFromPattern pat = S.singleton (void pat)

-- | generate all the patterns, then remove ourselves
generate :: Pattern (Type ann) -> S.Set (Pattern ())
generate pat = S.difference (generateFromPattern pat) (S.singleton $ void pat)
