{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Typecheck.Patterns.Generate
  ( generate,
  )
where

import Calc.Typecheck.Types
import Calc.Types.Constructor
import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import Data.Functor (void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S

generateFromType :: Type ann -> S.Set (Pattern ())
generateFromType (TPrim _ TBool) = S.fromList [PLiteral () (PBool True), PLiteral () (PBool False)]
generateFromType (TPrim _ _) = S.singleton (PWildcard ())
generateFromType _ = mempty

-- generateFromType, except if we get nothing, we return PWildcard,
-- to stop cartesian product being eliminated by empty value
safeGenerateFromType :: Type ann -> S.Set (Pattern ())
safeGenerateFromType ty =
  let pats = generateFromType ty
   in if S.null pats then S.singleton (PWildcard ()) else pats

-- generateFromPattern, except if we get nothing, we return ourselves,
-- to stop cartesian product being eliminated by empty value
safeGenerateFromPattern :: TypecheckEnv ann -> Pattern (Type ann) -> S.Set (Pattern ())
safeGenerateFromPattern env pat =
  let pats = generateFromPattern env pat
   in if S.null pats then S.singleton (void pat) else pats

generateFromPattern :: TypecheckEnv ann -> Pattern (Type ann) -> S.Set (Pattern ())
generateFromPattern _ (PLiteral ty _) =
  generateFromType ty
generateFromPattern env (PTuple _ty patA patAs) =
  let pats = patA : NE.toList patAs
      manyRows = traverse (fmap void . S.toList . safeGenerateFromPattern env) pats
      fromRow patList =
        case patList of
          (a : b : bs) -> S.singleton $ PTuple () a (b NE.:| bs)
          other -> error (show other)
   in foldMap fromRow manyRows
generateFromPattern env@(TypecheckEnv {tceDataTypes}) (PConstructor _ty constructor pats) =
  let generatedPatsForThisConstructor =
        traverse (fmap void . S.toList . safeGenerateFromPattern env) pats
      fromRow patList =
        S.singleton $ PConstructor () constructor patList
      patternsForThisConstructors =
        foldMap fromRow generatedPatsForThisConstructor

      otherConstructors = findMatches tceDataTypes constructor
      otherPatterns =
        foldMap
          ( \(cn, tys) ->
              let generatedPats = traverse (fmap void . S.toList . safeGenerateFromType) tys
                  fromRow2 patList =
                    S.singleton $ PConstructor () cn patList
               in foldMap fromRow2 generatedPats
          )
          otherConstructors
   in patternsForThisConstructors <> otherPatterns
generateFromPattern _ pat = S.singleton (void pat)

findMatches ::
  M.Map Constructor (TCDataType ann) ->
  Constructor ->
  S.Set (Constructor, [Type ()])
findMatches tceDataTypes constructor =
  case M.lookup constructor tceDataTypes of
    Just (TCDataType {tcdtName}) ->
      let matchesDataName (TCDataType {tcdtName = thisDataType}) =
            tcdtName == thisDataType
          extract (cn, TCDataType {tcdtArgs}) = (cn, void <$> tcdtArgs)
       in S.fromList $ extract <$> M.toList (M.filter matchesDataName tceDataTypes)
    Nothing -> error "sdfsdf"

-- | generate all the patterns, then remove ourselves
generate :: TypecheckEnv ann -> Pattern (Type ann) -> S.Set (Pattern ())
generate env pat =
  S.difference
    (generateFromPattern env pat)
    (S.singleton $ void pat)
