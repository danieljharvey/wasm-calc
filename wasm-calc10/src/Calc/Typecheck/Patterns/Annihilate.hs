{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Typecheck.Patterns.Annihilate
  ( annihilatePattern,
  )
where

import Calc.Types.Pattern
import Data.Functor (void)
import qualified Data.List.NonEmpty as NE
import Data.Monoid

-- do all left patterns make right patterns redundent
annihilateAll ::
  [(Pattern (), Pattern ())] ->
  Bool
annihilateAll =
  foldr
    (\(a, b) keep -> keep && annihilate a b)
    True

-- | does left make right redundent
-- if l == '_' and r == `True`, then yes, for instance
annihilate :: Pattern () -> Pattern () -> Bool
annihilate l r =
  case (l, r) of
    (a, b) | a == b -> True
    (PWildcard _, _) -> True -- wildcard trumps all
    (PVar _ _, _) -> True -- as does var
    (PTuple _ a as, PTuple _ b bs) ->
      let allPairs = zip ([a] <> NE.toList as) ([b] <> NE.toList bs)
       in annihilateAll allPairs
    {-    (PConstructor _ tyConA argsA, PConstructor _ tyConB argsB) ->
    (tyConA == tyConB)
      && annihilateAll
        (zip argsA argsB) -}
    (PTuple _ a as, _) ->
      isComplete a && getAll (foldMap (All . isComplete) as)
    _ -> False

-- is this item total, as such, ie, is it always true?
isComplete :: Pattern ann -> Bool
isComplete (PWildcard _) = True
isComplete (PVar _ _) = True
isComplete (PTuple _ a as) =
  isComplete a
    && getAll (foldMap (All . isComplete) (NE.toList as))
isComplete _ = False

annihilatePattern :: Pattern a -> [Pattern b] -> [Pattern b]
annihilatePattern pat =
  filter
    ( not
        . annihilate
          (void pat)
        . void
    )
