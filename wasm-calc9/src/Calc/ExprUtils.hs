{-# LANGUAGE RankNTypes #-}

module Calc.ExprUtils
  ( mapOuterExprAnnotation,
    getOuterAnnotation,
    bindExpr,
  mapExpr,
    getOuterPatternAnnotation,
  )
where

import           Calc.Types
import           Control.Monad.Identity

-- | get the annotation in the first leaf found in an `Expr`.
-- useful for getting the overall type of an expression
getOuterAnnotation :: Expr ann -> ann
getOuterAnnotation (EAnn ann _ _)     = ann
getOuterAnnotation (EInfix ann _ _ _) = ann
getOuterAnnotation (EPrim ann _)      = ann
getOuterAnnotation (ELet ann _ _ _)   = ann
getOuterAnnotation (EIf ann _ _ _)    = ann
getOuterAnnotation (EVar ann _)       = ann
getOuterAnnotation (EApply ann _ _)   = ann
getOuterAnnotation (ETuple ann _ _)   = ann
getOuterAnnotation (EBox ann _)       = ann
getOuterAnnotation (ELoad ann _)      = ann
getOuterAnnotation (EStore ann _ _)   = ann
getOuterAnnotation (ESet ann _ _)     = ann
getOuterAnnotation (EBlock ann _)     = ann

-- | modify the outer annotation of an expression
-- useful for adding line numbers during parsing
mapOuterExprAnnotation :: (ann -> ann) -> Expr ann -> Expr ann
mapOuterExprAnnotation f expr' =
  case expr' of
    EAnn ann a b     -> EAnn (f ann) a b
    EInfix ann a b c -> EInfix (f ann) a b c
    EPrim ann a      -> EPrim (f ann) a
    ELet ann a b c   -> ELet (f ann) a b c
    EIf ann a b c    -> EIf (f ann) a b c
    EVar ann a       -> EVar (f ann) a
    EApply ann a b   -> EApply (f ann) a b
    ETuple ann a b   -> ETuple (f ann) a b
    EBox ann a       -> EBox (f ann) a
    ELoad ann a      -> ELoad (f ann) a
    EStore ann a b   -> EStore (f ann) a b
    ESet ann a b     -> ESet (f ann) a b
    EBlock ann a     -> EBlock (f ann) a

mapExpr :: (Expr ann -> Expr ann) -> Expr ann -> Expr ann
mapExpr f
  = runIdentity . bindExpr  (Identity . f)

-- | Given a function that changes `Expr` values to `m Expr`, apply it throughout
-- an AST tree
bindExpr :: (Applicative m) => (Expr ann -> m (Expr ann)) -> Expr ann -> m (Expr ann)
bindExpr f (EInfix ann op a b) =
  EInfix ann op <$> f a <*> f b
bindExpr _ (EPrim ann a) =
  pure $ EPrim ann a
bindExpr f (ELet ann ident a b) =
  ELet ann ident <$> f a <*> f b
bindExpr _ (EVar ann a) =
  pure $ EVar ann a
bindExpr f (EApply ann fn args) =
  EApply ann fn <$> traverse f args
bindExpr f (EIf ann predExpr thenExpr elseExpr) =
  EIf ann <$> f predExpr <*> f thenExpr <*> f elseExpr
bindExpr f (ETuple ann a as) =
  ETuple ann <$> f a <*> traverse f as
bindExpr f (EBox ann a) = EBox ann <$> f a
bindExpr f (EAnn ann a b) = EAnn ann a <$> f b
bindExpr f (ELoad ann a) = ELoad ann <$> f a
bindExpr f (EStore ann a b) = EStore ann <$> f a <*> f b
bindExpr f (ESet ann a b) = ESet ann a <$> f b
bindExpr f (EBlock ann a) = EBlock ann <$> f a

getOuterPatternAnnotation :: Pattern ann -> ann
getOuterPatternAnnotation (PWildcard ann)  = ann
getOuterPatternAnnotation (PVar ann _)     = ann
getOuterPatternAnnotation (PTuple ann _ _) = ann
getOuterPatternAnnotation (PBox ann _)     = ann
