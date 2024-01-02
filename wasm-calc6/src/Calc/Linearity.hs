{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Linearity (getUses, Linearity (..)) where

import Calc.ExprUtils
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Type
import Control.Monad.State
import qualified Data.Map as M
import GHC.Natural

-- | Primitive values can be used as much as you like, don't care
data Linearity
  = Primitive
  | Boxed Natural
  deriving stock (Eq, Ord, Show)

instance Semigroup Linearity where
  (Boxed a) <> (Boxed b) = Boxed (a + b)
  _ <> _ = Primitive

instance Monoid Linearity where
  mempty = Boxed 0

newtype LinearState = LinearState {lsUses :: M.Map Identifier Linearity}

getUses :: Function (Type ann) -> M.Map Identifier Linearity
getUses (Function {fnBody, fnArgs}) =
  lsUses $
    execState
      (decorateWithUses fnBody)
      (LinearState {lsUses = initialUses})
  where
    initialUses =
      foldMap
        ( \(ArgumentName arg, ty) ->
            M.singleton (Identifier arg) $ case ty of
              TPrim {} -> Primitive
              _ -> Boxed 0
        )
        fnArgs

recordUse :: (MonadState LinearState m) => Identifier -> m ()
recordUse ident =
  modify (\ls -> ls {lsUses = M.insertWith mappend ident (Boxed 1) (lsUses ls)})

addLetBinding :: (MonadState LinearState m) => Identifier -> Type ann -> m ()
addLetBinding ident ty =
  let initialLinearity = case ty of
        TPrim {} -> Primitive
        _ -> Boxed 0
   in modify (\ls -> ls {lsUses = M.insertWith mappend ident initialLinearity (lsUses ls)})

decorateWithUses :: (MonadState LinearState m) => Expr (Type ann) -> m (Expr (Type ann))
decorateWithUses (EVar ann ident) = do
  recordUse ident
  pure (EVar ann ident)
decorateWithUses (ELet ann ident expr rest) = do
  addLetBinding ident (getOuterAnnotation expr)
  ELet ann ident <$> decorateWithUses expr <*> decorateWithUses rest
decorateWithUses other =
  bindExpr decorateWithUses other
