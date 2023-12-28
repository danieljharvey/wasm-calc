module Calc.TypeUtils (bindType, mapOuterTypeAnnotation, getOuterTypeAnnotation) where

import Calc.Types.Type

getOuterTypeAnnotation :: Type ann -> ann
getOuterTypeAnnotation (TPrim ann _) = ann
getOuterTypeAnnotation (TFunction ann _ _) = ann
getOuterTypeAnnotation (TTuple ann _ _) = ann
getOuterTypeAnnotation (TVar ann _) = ann
getOuterTypeAnnotation (TUnificationVar ann _) = ann

mapOuterTypeAnnotation :: (ann -> ann) -> Type ann -> Type ann
mapOuterTypeAnnotation f (TPrim ann p) = TPrim (f ann) p
mapOuterTypeAnnotation f (TFunction ann a b) = TFunction (f ann) a b
mapOuterTypeAnnotation f (TTuple ann a b) = TTuple (f ann) a b
mapOuterTypeAnnotation f (TVar ann v) = TVar (f ann) v
mapOuterTypeAnnotation f (TUnificationVar ann v) = TUnificationVar (f ann) v

bindType :: (Applicative m) => (Type ann -> m (Type ann)) -> Type ann -> m (Type ann)
bindType _ (TPrim ann p) =
  pure $ TPrim ann p
bindType f (TFunction ann a b) =
  TFunction ann <$> traverse f a <*> f b
bindType f (TTuple ann a as) =
  TTuple ann <$> f a <*> traverse f as
bindType _ (TVar ann a) =
  pure $ TVar ann a
bindType _ (TUnificationVar ann a) =
  pure $ TUnificationVar ann a
