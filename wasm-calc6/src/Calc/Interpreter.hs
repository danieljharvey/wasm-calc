{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Interpreter
  ( runInterpreter,
    interpret,
    interpretModule,
    InterpreterError (..),
    InterpreterState (..),
    InterpreterEnv (..),
  )
where

import Calc.Types
import Control.Monad (when, zipWithM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Natural

-- | type for interpreter state
newtype InterpreterState ann = InterpreterState
  { isFunctions :: Map FunctionName (Function ann)
  }

-- | type of errors that can occur
data InterpreterError ann
  = NonBooleanPredicate ann (Expr ann)
  | FunctionNotFound FunctionName [FunctionName]
  | VarNotFound Identifier [Identifier]
  | AccessNonTuple (Expr ann)
  | AccessOutsideTupleBounds (Expr ann) Natural
  | PatternMismatch (Expr ann) (Pattern ann)
  deriving stock (Eq, Ord, Show)

-- | type of Reader env for interpreter state
-- we use this for scoped temporary state
newtype InterpreterEnv ann = InterpreterEnv
  { ieVars :: Map Identifier (Expr ann)
  }

newtype InterpretM ann a = InterpretM {runInterpretM :: ReaderT (InterpreterEnv ann) (StateT (InterpreterState ann) (Either (InterpreterError ann))) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError (InterpreterError ann),
      MonadState (InterpreterState ann),
      MonadReader (InterpreterEnv ann)
    )

runInterpreter ::
  InterpretM ann a ->
  Either (InterpreterError ann) a
runInterpreter = flip evalStateT initialState . flip runReaderT initialEnv . runInterpretM
  where
    initialEnv = InterpreterEnv mempty
    initialState = InterpreterState mempty

-- | run an `InterpretM` action, after adding some arguments into the
-- Reader environment
-- we use the Reader env here because the vars disappear after we use them,
-- say, in a function
withVars ::
  [(Identifier, Expr ann)] ->
  InterpretM ann a ->
  InterpretM ann a
withVars newVars =
  local
    ( \(InterpreterEnv ieVars) ->
        InterpreterEnv $ ieVars <> M.fromList newVars
    )

-- | lookup a variable in the Reader environment
lookupVar :: Identifier -> InterpretM ann (Expr ann)
lookupVar identifier = do
  maybeValue <- asks (M.lookup identifier . ieVars)
  case maybeValue of
    Just expr -> pure expr
    Nothing -> do
      allVars <- asks (M.keys . ieVars)
      throwError (VarNotFound identifier allVars)

interpretInfix ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  InterpretM ann (Expr ann)
-- ints
interpretInfix ann OpAdd (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a + b)
interpretInfix ann OpSubtract (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a - b)
interpretInfix ann OpMultiply (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a * b)
-- float
interpretInfix ann OpAdd (EPrim _ (PFloat a)) (EPrim _ (PFloat b)) =
  pure $ EPrim ann (PFloat $ a + b)
interpretInfix ann OpSubtract (EPrim _ (PFloat a)) (EPrim _ (PFloat b)) =
  pure $ EPrim ann (PFloat $ a - b)
interpretInfix ann OpMultiply (EPrim _ (PFloat a)) (EPrim _ (PFloat b)) =
  pure $ EPrim ann (PFloat $ a * b)
interpretInfix ann OpEquals (EPrim _ a) (EPrim _ b) =
  pure $ EPrim ann (PBool $ a == b)
interpretInfix ann op a b = do
  iA <- interpret a
  iB <- interpret b
  interpretInfix ann op iA iB

-- | look up the function, adds the arguments into the Reader environment
-- then interpret the function body
interpretApply :: FunctionName -> [Expr ann] -> InterpretM ann (Expr ann)
interpretApply fnName args = do
  fn <- gets (M.lookup fnName . isFunctions)
  case fn of
    Just (Function {fnArgs, fnBody}) ->
      withVars (zip (coerce . faName <$> fnArgs) args) (interpret fnBody)
    Nothing -> do
      allFnNames <- gets (M.keys . isFunctions)
      throwError (FunctionNotFound fnName allFnNames)

identifiersFromPattern :: Pattern ann -> Expr ann -> InterpretM ann [(Identifier, Expr ann)]
identifiersFromPattern (PVar _ identifier) expr = do
  iExpr <- interpret expr
  pure [(identifier, iExpr)]
identifiersFromPattern (PBox _ pat) (EBox _ expr) =
  identifiersFromPattern pat expr
identifiersFromPattern (PWildcard _) _ = pure mempty
identifiersFromPattern pat@(PTuple _ p ps) expr@(ETuple _ a as) = do
  iA <- interpret a
  iAs <- traverse interpret as
  when
    (length as /= length ps)
    (throwError $ PatternMismatch expr pat)
  allIdents <- zipWithM identifiersFromPattern (p : NE.toList ps) (iA : NE.toList iAs)
  pure $ mconcat allIdents
identifiersFromPattern pat ty =
  throwError $ PatternMismatch ty pat

-- | just keep reducing the thing until the smallest thing
interpret ::
  Expr ann ->
  InterpretM ann (Expr ann)
interpret (EPrim ann p) =
  pure (EPrim ann p)
interpret (EVar _ ident) =
  lookupVar ident
interpret (ELet _ pat expr rest) = do
  vars <- identifiersFromPattern pat expr
  withVars vars (interpret rest)
interpret (EApply _ fnName args) =
  interpretApply fnName args
interpret (EInfix ann op a b) =
  interpretInfix ann op a b
interpret (ETuple ann a as) = do
  aA <- interpret a
  asA <- traverse interpret as
  pure (ETuple ann aA asA)
interpret (EContainerAccess _ tup index) = do
  aTup <- interpret tup
  interpretTupleAccess aTup index
interpret (EIf ann predExpr thenExpr elseExpr) = do
  predA <- interpret predExpr
  case predA of
    (EPrim _ (PBool True)) -> interpret thenExpr
    (EPrim _ (PBool False)) -> interpret elseExpr
    other -> throwError (NonBooleanPredicate ann other)
interpret (EBox ann a) =
  EBox ann <$> interpret a

interpretTupleAccess :: Expr ann -> Natural -> InterpretM ann (Expr ann)
interpretTupleAccess wholeExpr@(ETuple _ fstExpr restExpr) index = do
  let items = zip ([0 ..] :: [Natural]) (fstExpr : NE.toList restExpr)
  case lookup (index - 1) items of
    Just expr -> pure expr
    Nothing -> throwError (AccessOutsideTupleBounds wholeExpr index)
interpretTupleAccess wholeExpr@(EBox _ innerExpr) index = do
  case index of
    1 -> interpret innerExpr
    _ -> throwError (AccessOutsideTupleBounds wholeExpr index)
interpretTupleAccess expr _ = throwError (AccessNonTuple expr)

interpretModule ::
  Module ann ->
  InterpretM ann (Expr ann)
interpretModule (Module {mdExpr, mdFunctions}) = do
  let fnMap = M.fromList $ (\fn -> (fnFunctionName fn, fn)) <$> mdFunctions
  put (InterpreterState fnMap)
  interpret mdExpr
