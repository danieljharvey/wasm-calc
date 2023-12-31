{-# LANGUAGE RankNTypes #-}

module Calc.ExprUtils
  ( mapOuterExprAnnotation,
    getOuterAnnotation,
    bindExpr,
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
getOuterAnnotation (EContainerAccess ann _ _) = ann
getOuterAnnotation (EBox ann _) = ann

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
    EContainerAccess ann a b -> EContainerAccess (f ann) a b
    EBox ann a -> EBox (f ann) a

-- | Given a function that changes `Expr` values to `m Expr`, apply it throughout
-- an AST tree
bindExpr :: (Applicative m) => (Expr ann -> m (Expr ann)) -> Expr ann -> m (Expr ann)
bindExpr f (EInfix ann op a b) =
  EInfix ann op <$> f a <*> f b
bindExpr _ (EPrim ann a) =
  pure $ EPrim ann a
bindExpr _ (EVar ann a) =
  pure $ EVar ann a
bindExpr f (EApply ann fn args) =
  EApply ann fn <$> traverse f args
bindExpr f (EIf ann predExpr thenExpr elseExpr) =
  EIf ann <$> f predExpr <*> f thenExpr <*> f elseExpr
bindExpr f (ETuple ann a as) =
  ETuple ann <$> f a <*> traverse f as
bindExpr f (EContainerAccess ann a nat) =
  EContainerAccess ann <$> f a <*> pure nat
bindExpr f (EBox ann a) = EBox ann <$> f a
