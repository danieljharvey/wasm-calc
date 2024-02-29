module Calc.TypeUtils
  ( bindType,
    mapType,
    mapOuterTypeAnnotation,
    getOuterTypeAnnotation,
  )
where

import           Calc.Types.Type
import           Control.Monad.Identity

getOuterTypeAnnotation :: Type ann -> ann
getOuterTypeAnnotation (TPrim ann _)           = ann
getOuterTypeAnnotation (TFunction ann _ _)     = ann
getOuterTypeAnnotation (TContainer ann _)      = ann
getOuterTypeAnnotation (TVar ann _)            = ann
getOuterTypeAnnotation (TUnificationVar ann _) = ann

mapOuterTypeAnnotation :: (ann -> ann) -> Type ann -> Type ann
mapOuterTypeAnnotation f (TPrim ann p)           = TPrim (f ann) p
mapOuterTypeAnnotation f (TFunction ann a b)     = TFunction (f ann) a b
mapOuterTypeAnnotation f (TContainer ann a)      = TContainer (f ann) a
mapOuterTypeAnnotation f (TVar ann v)            = TVar (f ann) v
mapOuterTypeAnnotation f (TUnificationVar ann v) = TUnificationVar (f ann) v

mapType :: (Type ann -> Type ann) -> Type ann -> Type ann
mapType f ty =
  runIdentity (bindType (pure . f) ty)

bindType ::
  (Applicative m) =>
  (Type ann -> m (Type ann)) ->
  Type ann ->
  m (Type ann)
bindType _ (TPrim ann p) =
  pure $ TPrim ann p
bindType f (TFunction ann a b) =
  TFunction ann <$> traverse f a <*> f b
bindType f (TContainer ann as) =
  TContainer ann <$> traverse f as
bindType _ (TVar ann a) =
  pure $ TVar ann a
bindType _ (TUnificationVar ann a) =
  pure $ TUnificationVar ann a
