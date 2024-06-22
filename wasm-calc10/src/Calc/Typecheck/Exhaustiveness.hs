{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Typecheck.Exhaustiveness
  ( PatternMatchError (..),
    validatePatterns,
    generate,
    missingPatterns,
    redundantPatterns,
    patternMatchErrorDiagnostic,
  )
where

import Calc.SourceSpan
import Calc.TypeUtils
import Calc.Types.Annotation
import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import Control.Monad.Except
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

data PatternMatchError ann
  = -- | Cases not covered in pattern matches
    -- | ann, [missing patterns]
    MissingPatterns ann [Pattern ()]
  | -- | Unnecessary cases covered by previous matches
    RedundantPatterns ann [Pattern ann]
  deriving stock (Eq, Ord, Show, Foldable)

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

--------------- annihiliate

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

---- bring it all together

-- filter outstanding items
filterMissing ::
  [Pattern ()] ->
  [Pattern ()] ->
  [Pattern ()]
filterMissing patterns required =
  nub $ foldr annihiliatePattern required patterns
  where
    annihiliatePattern pat =
      filter
        ( not
            . annihilate
              (void pat)
            . void
        )

-- | given a list of patterns, return a list of missing patterns
missingPatterns ::
  [Pattern (Type ann)] ->
  [Pattern ()]
missingPatterns patterns =
  let generated = mconcat $ generate <$> patterns
   in filterMissing (void <$> patterns) (S.toList generated)

----- what about redundent stuff?

redundantPatterns ::
  [Pattern (Type ann)] ->
  [Pattern (Type ann)]
redundantPatterns patterns = do
  let generated = mconcat $ generate <$> patterns
      annihiliatePattern pat =
        filter
          ( not
              . annihilate
                (void pat)
              . void
          )
      -- add index, the first pattern is never redundant
      patternsWithIndex = zip patterns ([0 ..] :: [Int])
   in snd $
        foldl'
          ( \(remaining, redundant) (pat, i) ->
              let rest = annihiliatePattern pat remaining
               in if length rest == length remaining && i > 0
                    then (rest, redundant <> [pat])
                    else (rest, redundant)
          )
          (S.toList generated, mempty)
          patternsWithIndex

positionFromAnnotation ::
  String ->
  T.Text ->
  Annotation ->
  Maybe Diag.Position
positionFromAnnotation path input ann =
  let toPos ss =
        Diag.Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
   in toPos <$> sourceSpan input ann

prettyPrint :: PP.Doc doc -> T.Text
prettyPrint = renderWithWidth 60

patternMatchErrorDiagnostic ::
  T.Text ->
  PatternMatchError Annotation ->
  Diag.Diagnostic T.Text
patternMatchErrorDiagnostic input e =
  let filename = "<repl>"
      diag = Diag.addFile mempty filename (T.unpack input)
      report = case e of
        (RedundantPatterns ann redundant) ->
          Diag.Err
            Nothing
            ( prettyPrint "Pattern match has unreachable patterns, you should remove them"
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This
                          ( prettyPrint $
                              PP.cat (PP.punctuate PP.line (PP.pretty <$> redundant))
                          )
                      )
                ]
            )
            []
        (MissingPatterns ann missing) ->
          Diag.Err
            Nothing
            ( prettyPrint "Pattern match is missing some patterns"
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This
                          ( prettyPrint $
                              PP.cat (PP.punctuate PP.line (PP.pretty <$> missing))
                          )
                      )
                ]
            )
            []
   in Diag.addReport diag report

renderWithWidth :: Int -> PP.Doc ann -> T.Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}
