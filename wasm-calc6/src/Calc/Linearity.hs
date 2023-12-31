{-# LANGUAGE FlexibleContexts #-}
module Calc.Linearity (getUses) where

import           Calc.ExprUtils
import           Calc.Types.Expr
import           Calc.Types.Identifier
import           Control.Monad.State
import qualified Data.Map              as M
import           Data.Monoid
import           GHC.Natural

newtype LinearState =
  LinearState { lsUses :: M.Map Identifier (Sum Natural) }

getUses :: Expr ann -> M.Map Identifier Natural
getUses expr = fmap getSum $ lsUses $ execState
    (decorateWithUses expr) (LinearState {lsUses = mempty})

recordUse :: (MonadState LinearState m) => Identifier -> m ()
recordUse ident =
  modify (\ls -> ls { lsUses = M.insertWith mappend ident (Sum 1) (lsUses ls) })

decorateWithUses :: (MonadState LinearState m) => Expr ann -> m (Expr ann)
decorateWithUses (EVar ann ident) = do
  recordUse ident
  pure (EVar ann ident)
decorateWithUses other =
  bindExpr decorateWithUses other
