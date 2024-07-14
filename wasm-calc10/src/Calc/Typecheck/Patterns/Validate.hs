{-# LANGUAGE FlexibleContexts #-}

module Calc.Typecheck.Patterns.Validate
  ( validatePatterns,
    missingPatterns,
    redundantPatterns,
  )
where

import Calc.TypeUtils
import Calc.Typecheck.Error.PatternMatchError
import Calc.Typecheck.Patterns.Annihilate
import Calc.Typecheck.Patterns.Generate
import Calc.Types.Pattern
import Calc.Types.Type
import Control.Monad.Except
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.List (nub)
import qualified Data.Set as S

validatePatterns ::
  ( MonadError (PatternMatchError ann) m
  ) =>
  ann ->
  [Pattern (Type ann)] ->
  m ()
validatePatterns ann patterns = do
  let missing = missingPatterns patterns
  case missing of
    [] -> pure ()
    _ ->
      throwError (MissingPatterns ann missing)
  let redundant = redundantPatterns patterns
  case redundant of
    [] -> pure ()
    _ -> do
      let redundantWithoutType = fmap getOuterTypeAnnotation <$> redundant
      throwError (RedundantPatterns ann redundantWithoutType)

-- | given a list of patterns, return a list of missing patterns
missingPatterns ::
  [Pattern (Type ann)] ->
  [Pattern ()]
missingPatterns patterns =
  let generated = mconcat $ generate <$> patterns
   in nub $ foldr (annihilatePattern . void) (S.toList generated) patterns

----- what about redundent stuff?

redundantPatterns ::
  [Pattern (Type ann)] ->
  [Pattern (Type ann)]
redundantPatterns patterns = do
  let generated = mconcat $ generate <$> patterns
      originalPatterns = void <$> patterns
      -- add index, the first pattern is never redundant
      patternsWithIndex = zip patterns ([0 ..] :: [Int])
   in snd $
        foldl'
          ( \(remaining, redundant) (pat, i) ->
              let rest = annihilatePattern pat remaining
               in if length rest == length remaining && i > 0
                    then (rest, redundant <> [pat])
                    else (rest, redundant)
          )
          (originalPatterns <> S.toList generated, mempty)
          patternsWithIndex
