{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Typecheck
  ( typecheck,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State, runState)
import Control.Monad.Writer
import Data.Map (Map)
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.TypedHoles
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type ElabM = ExceptT TypeError (WriterT [Constraint] (ReaderT Swaps (State TypecheckState)))

runElabM ::
  Swaps ->
  TypecheckState ->
  ElabM a ->
  Either TypeError ([Constraint], TypecheckState, a)
runElabM swaps tcState value =
  case either' of
    ((Right a, constraints), newTcState) -> Right (constraints, newTcState, a)
    ((Left e, _), _) -> Left e
  where
    either' =
      runState
        (runReaderT (runWriterT (runExceptT value)) swaps)
        tcState

-- run inference, and substitute everything possible
typecheck ::
  Map Name MonoType ->
  Swaps ->
  Environment ->
  Expr Variable Annotation ->
  Either TypeError (Substitutions, [Constraint], Expr Variable (MonoType, Annotation), MonoType)
typecheck typeMap swaps env expr = do
  let tcAction = do
        (elabExpr, constraints) <- listen (elab (envWithBuiltInTypes <> env) expr)
        subs <- solve constraints
        typedHolesCheck typeMap subs
        pure (subs, constraints, elabExpr)
  (_, _, (subs, constraints, tyExpr)) <- runElabM swaps defaultTcState tcAction
  let typedExpr = applySubst subs tyExpr
  pure (subs, constraints, typedExpr, getTypeFromAnn typedExpr)