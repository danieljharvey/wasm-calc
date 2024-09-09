module Calc.TypeUtils
  ( bindType,
    mapType,
    monoidType,
    mapOuterTypeAnnotation,
    getOuterTypeAnnotation,
  )
where

import Calc.Types.Type
import Control.Monad.Identity

getOuterTypeAnnotation :: Type ann -> ann
getOuterTypeAnnotation (TPrim ann _) = ann
getOuterTypeAnnotation (TFunction ann _ _) = ann
getOuterTypeAnnotation (TContainer ann _) = ann
getOuterTypeAnnotation (TVar ann _) = ann
getOuterTypeAnnotation (TUnificationVar ann _) = ann
getOuterTypeAnnotation (TConstructor ann _ _) = ann
getOuterTypeAnnotation (TArray ann _ _) = ann


mapOuterTypeAnnotation :: (ann -> ann) -> Type ann -> Type ann
mapOuterTypeAnnotation f (TPrim ann p) = TPrim (f ann) p
mapOuterTypeAnnotation f (TFunction ann a b) = TFunction (f ann) a b
mapOuterTypeAnnotation f (TContainer ann a) = TContainer (f ann) a
mapOuterTypeAnnotation f (TVar ann v) = TVar (f ann) v
mapOuterTypeAnnotation f (TUnificationVar ann v) = TUnificationVar (f ann) v
mapOuterTypeAnnotation f (TConstructor ann a b) = TConstructor (f ann) a b
mapOuterTypeAnnotation f (TArray ann nat a) = TArray (f ann) nat a


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
bindType f (TConstructor ann dn args) =
  TConstructor ann dn <$> traverse f args
bindType f (TArray ann nat a)
 = TArray ann nat <$> f a

monoidType :: (Monoid m) => (Type ann -> m) -> Type ann -> m
monoidType _ (TPrim {}) = mempty
monoidType f (TFunction _ args ret) = foldMap f args <> f ret
monoidType f (TContainer _ as) = foldMap f as
monoidType _ (TVar {}) = mempty
monoidType _ (TUnificationVar {}) = mempty
monoidType f (TConstructor _ _ args) = foldMap f args
monoidType f (TArray _ _ a) = f a
