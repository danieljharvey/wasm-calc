{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.Interpret
  ( interpretWithScope,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Applicative
import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Language.Mimsa.Interpreter.SwapName
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Library
import Language.Mimsa.Types

useVar :: Variable -> App (Expr Variable)
useVar (NumberedVar i) = do
  scope' <- readScope
  case M.lookup (NumberedVar i) (getScope scope') of
    Just expr -> instantiateVar expr
    Nothing -> throwError $ CouldNotFindVar scope' (NumberedVar i)
useVar (NamedVar n) = do
  scope' <- readScope
  case M.lookup (NamedVar n) (getScope scope') of
    Just expr -> instantiateVar expr
    Nothing -> throwError $ CouldNotFindVar scope' (NamedVar n)
useVar (BuiltIn n) =
  case getLibraryFunction (BuiltIn n) of
    Just ff -> unwrapBuiltIn n ff
    Nothing -> do
      scope' <- readScope
      throwError $ CouldNotFindBuiltIn scope' (BuiltIn n)
useVar var@(BuiltInActual n ids) =
  case getLibraryFunction (BuiltIn n) of
    Just ff -> runBuiltIn ids ff
    Nothing -> do
      scope' <- readScope
      throwError $ CouldNotFindBuiltIn scope' var

-- make a fresh copy for us to use
-- is this necessary?
instantiateVar :: Expr Variable -> App (Expr Variable)
instantiateVar expr = case expr of
  (MyLambda binder expr') -> do
    (freshBinder, freshExpr) <- newLambdaCopy binder expr'
    interpretWithScope (MyLambda freshBinder freshExpr)
  other -> interpretWithScope other

runBuiltIn :: BiIds -> ForeignFunc -> App (Expr Variable)
runBuiltIn _ (NoArgs _ io) = liftIO io
runBuiltIn (OneId v1) (OneArg _ io) = do
  expr1 <- useVar v1
  liftIO (io expr1)
runBuiltIn (TwoIds v1 v2) (TwoArgs _ io) = do
  expr1 <- useVar v1
  expr2 <- useVar v2
  liftIO (io expr1 expr2)
runBuiltIn (ThreeIds v1 v2 v3) (ThreeArgs _ io) = do
  expr1 <- useVar v1
  expr2 <- useVar v2
  expr3 <- useVar v3
  liftIO (io expr1 expr2 expr3)
runBuiltIn ids _ = throwError $ CouldNotMatchBuiltInId ids

unwrapBuiltIn :: Name -> ForeignFunc -> App (Expr Variable)
unwrapBuiltIn name (NoArgs _ _) = do
  let actual = BuiltInActual name NoId
  addToScope (Scope $ M.singleton actual (MyVar (BuiltIn name)))
  pure (MyVar actual)
unwrapBuiltIn name (OneArg _ _) = do
  v1 <- nextVariable
  let actual = BuiltInActual name (OneId v1)
  addToScope (Scope $ M.singleton actual (MyVar (BuiltIn name))) -- add new name to scope
  pure
    ( MyLambda v1 (MyVar actual)
    )
unwrapBuiltIn name (TwoArgs _ _) = do
  v1 <- nextVariable
  v2 <- nextVariable
  let actual = BuiltInActual name (TwoIds v1 v2)
  addToScope (Scope $ M.singleton actual (MyVar (BuiltIn name))) -- add new name to scope
  pure
    ( MyLambda
        v1
        ( MyLambda
            v2
            (MyVar actual)
        )
    )
unwrapBuiltIn name (ThreeArgs _ _) = do
  v1 <- nextVariable
  v2 <- nextVariable
  v3 <- nextVariable
  let actual = BuiltInActual name (ThreeIds v1 v2 v3)
  addToScope (Scope $ M.singleton actual (MyVar (BuiltIn name))) -- add new name to scope
  pure
    ( MyLambda
        v1
        ( MyLambda
            v2
            ( MyLambda
                v3
                (MyVar actual)
            )
        )
    )

-- get new var
newLambdaCopy :: Variable -> Expr Variable -> App (Variable, Expr Variable)
newLambdaCopy name expr = do
  newName' <- nextVariable
  newExpr <- swapName name newName' expr
  pure (newName', newExpr)

interpretWithScope :: Expr Variable -> App (Expr Variable)
interpretWithScope interpretExpr =
  case interpretExpr of
    (MyLiteral a) -> pure (MyLiteral a)
    (MyPair a b) -> do
      exprA <- interpretWithScope a
      exprB <- interpretWithScope b
      pure (MyPair exprA exprB)
    (MyLet binder expr body) -> do
      addToScope (Scope $ M.singleton binder expr)
      interpretWithScope body
    (MyLetPair binderA binderB (MyPair a b) body) -> do
      let newScopes = Scope $ M.fromList [(binderA, a), (binderB, b)]
      addToScope newScopes
      interpretWithScope body
    (MyLetPair binderA binderB (MyVar v) body) -> do
      expr <- interpretWithScope (MyVar v)
      interpretWithScope (MyLetPair binderA binderB expr body)
    (MyLetPair _ _ a _) ->
      throwError $ CannotDestructureAsPair a
    (MyVar var) -> do
      scope <- readScope
      (useVar var >>= interpretWithScope)
        <|> throwError (CouldNotFindVar scope var)
    (MyCase (MySum MyLeft a) (MyLambda binderL exprL) _) ->
      interpretWithScope (MyLet binderL a exprL)
    (MyCase (MySum MyRight b) _ (MyLambda binderR exprR)) ->
      interpretWithScope (MyLet binderR b exprR)
    (MyCase (MyVar a) l r) -> do
      expr <- interpretWithScope (MyVar a)
      interpretWithScope (MyCase expr l r)
    (MyCase (MyApp a b) l r) -> do
      expr <- interpretWithScope (MyApp a b)
      interpretWithScope (MyCase expr l r)
    (MyCase a _ _) -> throwError $ CannotDestructureAsSum a
    (MyApp (MyVar f) value) -> do
      expr <- interpretWithScope (MyVar f)
      interpretWithScope (MyApp expr value)
    (MyApp (MyLambda binder expr) value) ->
      interpretWithScope (MyLet binder value expr)
    (MyApp other value) -> do
      expr <- interpretWithScope other
      interpretWithScope (MyApp expr value)
    (MyLetList binderHead binderRest (MyList as) body) -> do
      let (listHead, listTail) = NE.uncons as
          tail' = case listTail of
            Nothing -> MySum MyLeft (MyLiteral MyUnit)
            Just bs -> MySum MyRight (MyList bs)
      let newScopes = Scope $ M.fromList [(binderHead, listHead), (binderRest, tail')]
      addToScope newScopes
      interpretWithScope body
    (MyRecordAccess (MyRecord record) name) ->
      case M.lookup name record of
        Just item -> interpretWithScope item
        _ -> throwError $ CannotFindMemberInRecord record name
    (MyRecordAccess (MyVar a) name) -> do
      expr <- interpretWithScope (MyVar a)
      interpretWithScope (MyRecordAccess expr name)
    (MyRecordAccess (MyRecordAccess a name') name) -> do
      expr <- interpretWithScope (MyRecordAccess a name')
      interpretWithScope (MyRecordAccess expr name)
    (MyRecordAccess a name) ->
      throwError $ CannotDestructureAsRecord a name
    (MyLetList binderHead binderRest (MyVar b) body) -> do
      expr <- interpretWithScope (MyVar b)
      interpretWithScope (MyLetList binderHead binderRest expr body)
    (MyLetList _ _ a _) ->
      throwError $ CannotDestructureAsList a
    (MySum s a) -> do
      expr <- interpretWithScope a
      pure (MySum s expr)
    (MyList as) -> do
      exprs <- traverse interpretWithScope as
      pure (MyList exprs)
    (MyRecord as) -> do
      exprs <- traverse interpretWithScope as
      pure (MyRecord exprs)
    (MyLambda a b) -> pure (MyLambda a b)
    (MyIf (MyLiteral (MyBool pred')) true false) ->
      if pred'
        then interpretWithScope true
        else interpretWithScope false
    (MyIf all'@(MyLiteral _) _ _) ->
      throwError $ PredicateForIfMustBeABoolean all'
    (MyIf all'@(MyLambda _ _) _ _) ->
      throwError $ PredicateForIfMustBeABoolean all'
    (MyIf pred' true false) -> do
      predExpr <- interpretWithScope pred'
      interpretWithScope (MyIf predExpr true false)
