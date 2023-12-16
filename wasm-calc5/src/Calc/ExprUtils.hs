{-# LANGUAGE RankNTypes #-}

module Calc.ExprUtils
  ( mapOuterExprAnnotation,
    getOuterAnnotation,
  )
where

import Calc.Types

-- | get the annotation in the first leaf found in an `Expr`.
-- useful for getting the overall type of an expression
getOuterAnnotation :: Expr ann -> ann
getOuterAnnotation (EInfix ann _ _ _) = ann
getOuterAnnotation (EPrim ann _) = ann
getOuterAnnotation (EIf ann _ _ _) = ann
getOuterAnnotation (EVar ann _) = ann
getOuterAnnotation (EApply ann _ _) = ann
getOuterAnnotation (ETuple ann _ _) = ann
getOuterAnnotation (ETupleAccess ann _ _) = ann

-- | modify the outer annotation of an expression
-- useful for adding line numbers during parsing
mapOuterExprAnnotation :: (ann -> ann) -> Expr ann -> Expr ann
mapOuterExprAnnotation f expr' =
  case expr' of
    EInfix ann a b c -> EInfix (f ann) a b c
    EPrim ann a -> EPrim (f ann) a
    EIf ann a b c -> EIf (f ann) a b c
    EVar ann a -> EVar (f ann) a
    EApply ann a b -> EApply (f ann) a b
    ETuple ann a b -> ETuple (f ann) a b
    ETupleAccess ann a b -> ETupleAccess (f ann) a b
