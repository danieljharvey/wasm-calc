module Calc.Typecheck.Infer
  ( infer,
    check,
  )
where

import qualified Data.Map.Strict as M
import Calc.ExprUtils
import Calc.TypeUtils
import Calc.Typecheck.Error
import Calc.Typecheck.Helpers
import Calc.Typecheck.Substitute
import Calc.Typecheck.Types
import Calc.Typecheck.Unify
import Calc.Types
import Control.Monad (unless, when, zipWithM, zipWithM_)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import qualified Data.List.NonEmpty as NE

check :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
check ty (EApply ann fn args) =
  checkApply (Just ty) ann fn args
check ty (EInfix ann op a b) =
  checkInfix (Just ty) ann op a b
check ty (ETuple ann fstExpr restExpr) =
  checkTuple (Just ty) ann fstExpr restExpr
check ty (ELet ann pat expr rest) =
  checkLet (Just ty) ann pat expr rest
check ty (EIf ann predExpr thenExpr elseExpr) =
  checkIf (Just ty) ann predExpr thenExpr elseExpr
check ty (ELoad ann index) =
  checkLoad (Just ty) ann index
check ty (EStore ann index expr) =
  checkStore (Just ty) ann index expr
check ty (ESet ann ident expr) =
  checkSet (Just ty) ann ident expr
check ty (EPrim _ (PFloatLit f)) = do
  tyPrim <- case ty of
    TPrim _ TFloat32 -> pure ty
    TPrim _ TFloat64 -> pure ty
    _ -> error "non float type error"
  pure $ EPrim tyPrim (PFloatLit f)
check ty (EPrim _ (PIntLit i)) = do
  tyPrim <- case ty of
    TPrim _ TInt8 -> pure ty
    TPrim _ TInt16 -> pure ty
    TPrim _ TInt32 -> pure ty
    TPrim _ TInt64 -> pure ty
    _ -> error "non int type error"
  pure $ EPrim tyPrim (PIntLit i)
check (TContainer tyAnn tyItems) (EBox _ inner) | length tyItems == 1 = do
  typedInner <- check (NE.head tyItems) inner
  let ty = TContainer tyAnn (NE.singleton (getOuterAnnotation typedInner))
  pure $ EBox ty typedInner
check ty expr = do
  exprA <- infer expr
  unifiedTy <- unify ty (getOuterAnnotation exprA)
  pure (mapOuterExprAnnotation (const unifiedTy) exprA)

checkLoad :: Maybe (Type ann) -> ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
checkLoad Nothing _ann _index =
  error "Can't infer ELoad type"
checkLoad (Just ty) ann index = do
  typedIndex <- check (TPrim ann TInt32) index
  _memLimit <- asks tceMemoryLimit
  {-
  unless (index < memLimit) $
    throwError $
      ManualMemoryAccessOutsideLimit ann memLimit index
  -}
  if isNumber ty
    then pure $ ELoad (ty $> ann) typedIndex
    else error "can only load primitive values"

-- | store always returns Void
checkStore ::
  Maybe (Type ann) ->
  ann ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
checkStore maybeTy ann index expr = do
  typedIndex <- check (TPrim ann TInt32) index
  typedExpr <- infer expr
  {-
  memLimit <- asks tceMemoryLimit
  unless (index < memLimit) $
    throwError $
      ManualMemoryAccessOutsideLimit ann memLimit index
  -}
  unless (isNumber $ getOuterAnnotation typedExpr) $
    error "can only store primitive values"
  let tyVoid = TPrim ann TVoid
  case maybeTy of
    Just ty -> void (unify tyVoid ty)
    Nothing -> pure ()
  pure $ EStore tyVoid typedIndex typedExpr

-- | set always returns Void
checkSet ::
  Maybe (Type ann) ->
  ann ->
  Identifier ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
checkSet maybeTy ann ident expr = do
  (TypecheckGlobal tyVar mutability) <- lookupGlobal ann ident
  case mutability of
    Constant -> throwError $ CantSetConstant ann ident
    Mutable -> pure ()
  typedExpr <- check tyVar expr
  let tyVoid = TPrim ann TVoid
  case maybeTy of
    Just ty -> void (unify tyVoid ty)
    Nothing -> pure ()
  pure $ ESet tyVoid ident typedExpr

checkIf ::
  Maybe (Type ann) ->
  ann ->
  Expr ann ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
checkIf maybeReturnTy ann predExpr thenExpr elseExpr = do
  predA <- infer predExpr
  case getOuterAnnotation predA of
    (TPrim _ TBool) -> pure ()
    otherType -> throwError (PredicateIsNotBoolean ann otherType)
  (thenA, elseA) <- case maybeReturnTy of
    Just returnTy -> do
      thenA <- check returnTy thenExpr
      elseA <- check returnTy elseExpr
      pure (thenA, elseA)
    Nothing -> do
      let thenThenElse = do
            thenA <- infer thenExpr
            elseA <- check (getOuterAnnotation thenA) elseExpr
            pure (thenA, elseA)

      let elseThenThen = do
            elseA <- infer elseExpr
            thenA <- check (getOuterAnnotation elseA) thenExpr
            pure (thenA, elseA)

      thenThenElse `catchError` const elseThenThen

  pure (EIf (getOuterAnnotation elseA) predA thenA elseA)

-- | any infix which is basically `a -> a -> Bool`
inferComparisonOperator ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
inferComparisonOperator ann op a b = do
  let leftThenRight = do
        elabA <- infer a
        elabB <- check (getOuterAnnotation elabA) b
        pure (elabA, elabB)
  let rightThenLeft = do
        elabB <- infer b
        elabA <- check (getOuterAnnotation elabB) a
        pure (elabA, elabB)
  (elabA, elabB) <-
    leftThenRight
      `catchError` const rightThenLeft
  let ty = TPrim ann TBool
  pure (EInfix ty op elabA elabB)

checkInfix ::
  Maybe (Type ann) ->
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
checkInfix _ ann OpEquals a b =
  inferComparisonOperator ann OpEquals a b
checkInfix _ ann OpGreaterThan a b =
  inferComparisonOperator ann OpGreaterThan a b
checkInfix _ ann OpGreaterThanOrEqualTo a b =
  inferComparisonOperator ann OpGreaterThanOrEqualTo a b
checkInfix _ ann OpLessThan a b =
  inferComparisonOperator ann OpLessThan a b
checkInfix _ ann OpLessThanOrEqualTo a b =
  inferComparisonOperator ann OpLessThanOrEqualTo a b
checkInfix _ ann OpAnd a b = do
  let ty = TPrim ann TBool
  elabA <- check ty a
  elabB <- check ty b
  pure (EInfix (ty $> ann) OpAnd elabA elabB)
checkInfix _ ann OpOr a b = do
  let ty = TPrim ann TBool
  elabA <- check ty a
  elabB <- check ty b
  pure (EInfix (ty $> ann) OpOr elabA elabB)
checkInfix (Just ty) ann op a b = do
  elabA <- check ty a
  elabB <- check ty b
  pure (EInfix (ty $> ann) op elabA elabB)
checkInfix Nothing ann op a b = do
  let throwInfixError = do
        elabA <- infer a
        elabB <- infer b
        throwError (InfixTypeMismatch op (getOuterAnnotation elabA) (getOuterAnnotation elabB))
  let leftThenRight = do
        elabA <- infer a
        elabB <- check (getOuterAnnotation elabA) b
        pure (elabA, elabB)
  let rightThenLeft = do
        elabB <- infer b
        elabA <- check (getOuterAnnotation elabB) a
        pure (elabA, elabB)
  (elabA, elabB) <-
    leftThenRight `catchError` \_ ->
      rightThenLeft `catchError` const throwInfixError
  let ty = getOuterAnnotation elabA
  unless
    (isNumber ty)
    ( throwError $
        InfixTypeMismatch op (getOuterAnnotation elabA) (getOuterAnnotation elabB)
    )
  pure (EInfix (ty $> ann) op elabA elabB)

-- | is this type a primitive number?
isNumber :: Type ann -> Bool
isNumber (TPrim _ TInt8) = True
isNumber (TPrim _ TInt16) = True
isNumber (TPrim _ TInt32) = True
isNumber (TPrim _ TInt64) = True
isNumber (TPrim _ TFloat32) = True
isNumber (TPrim _ TFloat64) = True
isNumber _ = False

-- | like `check`, but we also check we're not passing a non-boxed value to a
-- generic argument
checkApplyArg :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
checkApplyArg ty expr = do
  tyExpr <- check ty expr
  unifyPrimitives ty (getOuterAnnotation tyExpr)
  pure tyExpr

-- check that no primitives and polymorphic vars get combined
unifyPrimitives :: Type ann -> Type ann -> TypecheckM ann ()
unifyPrimitives (TUnificationVar {}) p@(TPrim {}) =
  throwError (NonBoxedGenericValue (getOuterTypeAnnotation p) p)
unifyPrimitives (TUnificationVar {}) _ = pure ()
unifyPrimitives (TFunction _ argA bodyA) (TFunction _ argB bodyB) = do
  zipWithM_ unifyPrimitives argA argB
  unifyPrimitives bodyA bodyB
unifyPrimitives (TContainer _ as) (TContainer _ bs) =
  zipWithM_ unifyPrimitives (NE.toList as) (NE.toList bs)
unifyPrimitives tyA tyB =
  if void tyA == void tyB
    then pure ()
    else throwError (TypeMismatch tyA tyB)

-- | if our return type is polymorphic, our concrete type should not be a
-- primitive
checkReturnType :: Type ann -> Type ann -> TypecheckM ann (Type ann)
checkReturnType (TUnificationVar {}) p@(TPrim ann _) =
  throwError (NonBoxedGenericValue ann p)
checkReturnType _ ty = pure ty

checkApply ::
  Maybe (Type ann) ->
  ann ->
  FunctionName ->
  [Expr ann] ->
  TypecheckM ann (Expr (Type ann))
checkApply maybeTy ann fnName args = do
  fn <- lookupFunction ann fnName
  (ty, elabArgs) <- case fn of
    TFunction _ tyFnArgs tyFnReturn -> do
      when
        (length args /= length tyFnArgs)
        ( throwError $
            FunctionArgumentLengthMismatch
              ann
              (length tyFnArgs)
              (length args)
        )

      -- what do we learn about args from return type?
      _ <- case maybeTy of
        Just ty -> void (unify ty tyFnReturn)
        Nothing -> pure ()

      unified <- gets tcsUnified

      -- what have we learned?
      let substitutedArgs = substitute unified <$> tyFnArgs

      elabArgs <- zipWithM checkApplyArg substitutedArgs args -- check each arg against type

      -- did we learn yet more?
      moreUnified <- gets tcsUnified

      actualTyReturn <-
        checkReturnType
          tyFnReturn
          (substitute moreUnified tyFnReturn)
      pure (actualTyReturn, elabArgs)
    _ ->
      throwError $
        NonFunctionTypeFound ann fn

  pure (EApply (ty $> ann) fnName elabArgs)

checkPattern :: Type ann -> Pattern ann -> TypecheckM ann (Pattern (Type ann))
checkPattern ty (PWildcard _) = pure (PWildcard ty)
checkPattern (TPrim _ TVoid) pat@(PVar _ _) =
  throwError (CantBindVoidValue pat)
checkPattern ty (PVar ann var) = pure (PVar (ty $> ann) var)
checkPattern ty@(TContainer _ tyItems) (PBox _ a)
  | length tyItems == 1 =
      PBox ty <$> checkPattern (NE.head tyItems) a
checkPattern ty@(TContainer _ tyItems) pat@(PTuple _ p ps) = do
  when
    (length (NE.tail tyItems) /= length ps)
    (throwError $ PatternMismatch ty pat)
  pHead <- checkPattern (NE.head tyItems) p
  pTail <- zipWithM checkPattern (NE.tail tyItems) (NE.toList ps)
  pure (PTuple ty pHead (NE.fromList pTail))
checkPattern ty pat = throwError $ PatternMismatch ty pat

checkTuple ::
  Maybe (Type ann) ->
  ann ->
  Expr ann ->
  NE.NonEmpty (Expr ann) ->
  TypecheckM ann (Expr (Type ann))
checkTuple (Just (TContainer _ tyItems)) ann fstExpr restExpr = do
  let tyFirst = NE.head tyItems
      tyRest = NE.fromList (NE.tail tyItems)
  typedFst <- check tyFirst fstExpr
  typedRest <- NE.fromList <$> zipWithM check (NE.toList tyRest) (NE.toList restExpr)
  let typ =
        TContainer
          ann
          ( NE.cons
              (getOuterAnnotation typedFst)
              (getOuterAnnotation <$> typedRest)
          )
  pure $ ETuple typ typedFst typedRest
checkTuple Nothing ann fstExpr restExpr = do
  typedFst <- infer fstExpr
  typedRest <- traverse infer restExpr
  let typ =
        TContainer
          ann
          ( NE.cons
              (getOuterAnnotation typedFst)
              (getOuterAnnotation <$> typedRest)
          )
  pure $ ETuple typ typedFst typedRest
checkTuple _ _ _ _ = error "tuple mess"

checkLet ::
  Maybe (Type ann) ->
  ann ->
  Pattern ann ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
checkLet maybeReturnTy ann pat expr rest = do
  typedExpr <- infer expr
  typedPat <- checkPattern (getOuterAnnotation typedExpr) pat
  typedRest <- withVar pat (getOuterAnnotation typedExpr) $
    case maybeReturnTy of
      Just returnTy -> check returnTy rest
      Nothing -> infer rest
  pure $ ELet (getOuterAnnotation typedRest $> ann) typedPat typedExpr typedRest

lookupConstructor :: ann -> Constructor ->
  TypecheckM ann (DataName, [TypeVar] ,[Type ann])
lookupConstructor ann constructor = do
  result <- asks (M.lookup constructor . tceDataTypes)
  case result of
    (Just (TCDataType dataType vars args)) -> pure (dataType,vars,args)
    Nothing -> throwError $ ConstructorNotFound ann constructor

infer :: Expr ann -> TypecheckM ann (Expr (Type ann))
infer (EAnn ann ty expr) = do
  typedExpr <- check ty expr
  pure $ EAnn (getOuterAnnotation typedExpr $> ann) (ty $> ty) typedExpr
infer (EPrim ann prim) =
  case prim of
    PBool _ -> pure (EPrim (TPrim ann TBool) prim)
    PIntLit _ -> throwError (UnknownIntegerLiteral ann)
    PFloatLit _ -> throwError (UnknownFloatLiteral ann)
infer (EBox ann inner) = do
  typedInner <- infer inner
  pure $
    EBox
      ( TContainer
          ann
          (NE.singleton $ getOuterAnnotation typedInner)
      )
      typedInner
infer (EConstructor ann constructor args) = do
  (dataType,vars, tyArgs) <- lookupConstructor ann constructor
  typedArgs <- traverse infer args
  monomorphisedArgs <-
    calculateMonomorphisedTypes vars tyArgs (getOuterAnnotation <$> typedArgs)
  let ty = TConstructor ann dataType (snd <$> monomorphisedArgs)
  pure (EConstructor ty constructor typedArgs)
infer (ELet ann pat expr rest) =
  checkLet Nothing ann pat expr rest
infer (EIf ann predExpr thenExpr elseExpr) =
  checkIf Nothing ann predExpr thenExpr elseExpr
infer (ETuple ann fstExpr restExpr) =
  checkTuple Nothing ann fstExpr restExpr
infer (EApply ann fnName args) =
  checkApply Nothing ann fnName args
infer (EVar ann var) = do
  ty <- lookupVar ann var
  pure (EVar (ty $> ann) var)
infer (EInfix ann op a b) =
  checkInfix Nothing ann op a b
infer (ELoad ann index) =
  checkLoad Nothing ann index
infer (EStore ann index expr) =
  checkStore Nothing ann index expr
infer (ESet ann ident expr) =
  checkSet Nothing ann ident expr
infer (EBlock ann expr) = do
  typedExpr <- infer expr
  pure $
    EBlock
      (mapOuterTypeAnnotation (const ann) (getOuterAnnotation typedExpr))
      typedExpr
