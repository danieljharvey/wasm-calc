{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calc.Typecheck.Elaborate
  ( infer,
    elaborateFunction,
    elaborateModule,
  )
where

import Calc.ExprUtils
import Calc.TypeUtils
import Calc.Typecheck.Error
import Calc.Typecheck.Helpers
import Calc.Typecheck.Substitute
import Calc.Typecheck.Types
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Import
import Calc.Types.Module
import Calc.Types.Op
import Calc.Types.Pattern
import Calc.Types.Prim
import Calc.Types.Type
import Control.Monad (when, zipWithM)
import Control.Monad.Except
import Control.Monad.State
import Data.Functor
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

elaborateModule ::
  forall ann.
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule (Module {mdImports, mdFunctions}) =
  runTypecheckM (TypecheckEnv mempty mempty) $ do
    imports <-
      traverse
        ( \imp -> do
            elabImport <- elaborateImport imp
            storeFunction (impImportName elabImport) mempty (impAnn elabImport)
            pure elabImport
        )
        mdImports

    fns <-
      traverse
        ( \fn -> do
            elabFn <- elaborateFunction fn
            storeFunction
              (fnFunctionName elabFn)
              (S.fromList $ fnGenerics fn)
              (fnAnn elabFn)
            pure elabFn
        )
        mdFunctions

    pure $
      Module
        { mdFunctions = fns,
          mdImports = imports
        }

elaborateImport :: Import ann -> TypecheckM ann (Import (Type ann))
elaborateImport
  Import
    { impArgs,
      impExternalModule,
      impReturnType,
      impAnn,
      impExternalFunction,
      impImportName
    } = do
    let importArguments =
          ( \ImportArg {iaName, iaType, iaAnn} ->
              ImportArg
                { iaName,
                  iaType = fmap (const iaType) iaType,
                  iaAnn = fmap (const iaAnn) iaType
                }
          )
            <$> impArgs

    let importType =
          TFunction
            impAnn
            (iaType <$> impArgs)
            impReturnType

    pure $
      Import
        { impImportName,
          impExternalModule,
          impExternalFunction,
          impAnn = importType,
          impArgs = importArguments,
          impReturnType = fmap (const impReturnType) impReturnType
        }

inferAndSubstitute ::
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
inferAndSubstitute expr = do
  exprA <- infer expr
  unified <- gets tcsUnified
  pure $ substitute unified <$> exprA

elaborateFunction ::
  Function ann ->
  TypecheckM ann (Function (Type ann))
elaborateFunction (Function {fnPublic, fnAnn, fnArgs, fnGenerics, fnFunctionName, fnBody}) = do
  exprA <-
    withFunctionEnv
      fnArgs
      (S.fromList fnGenerics)
      (inferAndSubstitute fnBody)
  let argsA =
        ( \FunctionArg {faName, faType, faAnn} ->
            FunctionArg
              { faName,
                faType = fmap (const faType) faType,
                faAnn = fmap (const faAnn) faType
              }
        )
          <$> fnArgs
  let tyFn =
        TFunction
          fnAnn
          (faType <$> fnArgs)
          (getOuterAnnotation exprA)
  pure
    ( Function
        { fnAnn = tyFn,
          fnGenerics,
          fnArgs = argsA,
          fnFunctionName = fnFunctionName,
          fnBody = exprA,
          fnPublic = fnPublic
        }
    )

check :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
check ty expr = do
  exprA <- infer expr
  unifiedTy <- unify ty (getOuterAnnotation exprA)
  pure (mapOuterExprAnnotation (const unifiedTy) exprA)

-- unification. for our simple purposes this means "smash two types
-- together and see what we learn" (or explode if it makes no sense)
unify :: Type ann -> Type ann -> TypecheckM ann (Type ann)
unify (TUnificationVar _ nat) b = do
  unifyVariableWithType nat b
unify a (TUnificationVar _ nat) = do
  unifyVariableWithType nat a
unify (TFunction ann argA bodyA) (TFunction _ argB bodyB) =
  TFunction ann
    <$> zipWithM unify argA argB
    <*> unify bodyA bodyB
unify (TContainer ann as) (TContainer _ bs) =
  TContainer ann
    <$> (NE.fromList <$> zipWithM unify (NE.toList as) (NE.toList bs))
unify tyA tyB =
  if void tyA == void tyB
    then pure tyA
    else throwError (TypeMismatch tyA tyB)

inferIf ::
  ann ->
  Expr ann ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
inferIf ann predExpr thenExpr elseExpr = do
  predA <- infer predExpr
  case getOuterAnnotation predA of
    (TPrim _ TBool) -> pure ()
    otherType -> throwError (PredicateIsNotBoolean ann otherType)
  thenA <- infer thenExpr
  elseA <- check (getOuterAnnotation thenA) elseExpr
  pure (EIf (getOuterAnnotation elseA) predA thenA elseA)

-- | any infix which is basically `a -> a -> Bool`
inferComparisonOperator ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
inferComparisonOperator ann op a b = do
  elabA <- infer a
  elabB <- infer b
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ tA, TPrim _ tB)
      | tA == tB ->
          -- if the types are the same, then great! it's a bool!
          pure (TPrim ann TBool)
    (otherA, otherB) ->
      -- otherwise, error!
      throwError (TypeMismatch otherA otherB)
  pure (EInfix ty op elabA elabB)

inferInfix ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
inferInfix ann OpEquals a b =
  inferComparisonOperator ann OpEquals a b
inferInfix ann OpGreaterThan a b =
  inferComparisonOperator ann OpGreaterThan a b
inferInfix ann OpGreaterThanOrEqualTo a b =
  inferComparisonOperator ann OpGreaterThanOrEqualTo a b
inferInfix ann OpLessThan a b =
  inferComparisonOperator ann OpLessThan a b
inferInfix ann OpLessThanOrEqualTo a b =
  inferComparisonOperator ann OpLessThanOrEqualTo a b
inferInfix ann op a b = do
  elabA <- infer a
  elabB <- infer b
  -- all the other infix operators need to be Int -> Int -> Int
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ TInt32, TPrim _ TInt32) ->
      -- if the types are the same, then great! it's an int32!
      pure (TPrim ann TInt32)
    (TPrim _ TInt64, TPrim _ TInt64) ->
      -- if the types are the same, then great! it's an int!
      pure (TPrim ann TInt64)
    (TPrim _ TFloat32, TPrim _ TFloat32) ->
      -- if the types are the same, then great! it's a float!
      pure (TPrim ann TFloat32)
    (TPrim _ TFloat64, TPrim _ TFloat64) ->
      -- if the types are the same, then great! it's a float!
      pure (TPrim ann TFloat64)
    (otherA, otherB) -> throwError (InfixTypeMismatch op otherA otherB)
  pure (EInfix ty op elabA elabB)

-- | like `check`, but we also check we're not passing a non-boxed value to a
-- generic argument
checkApplyArg :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
checkApplyArg ty@(TUnificationVar {}) expr = do
  tyExpr <- infer expr
  case getOuterAnnotation tyExpr of
    p@TPrim {} -> throwError (NonBoxedGenericValue (getOuterTypeAnnotation p) p)
    _other -> check ty expr
checkApplyArg ty expr = check ty expr

-- | if our return type is polymorphic, our concrete type should not be a
-- primitive
checkReturnType :: Type ann -> Type ann -> TypecheckM ann (Type ann)
checkReturnType (TUnificationVar {}) p@(TPrim ann _) =
  throwError (NonBoxedGenericValue ann p)
checkReturnType _ ty = pure ty

inferApply ::
  ann ->
  FunctionName ->
  [Expr ann] ->
  TypecheckM ann (Expr (Type ann))
inferApply ann fnName args = do
  fn <- lookupFunction ann fnName
  (ty, elabArgs) <- case fn of
    TFunction _ tArgs tReturn -> do
      when
        (length args /= length tArgs)
        ( throwError $
            FunctionArgumentLengthMismatch
              ann
              (length tArgs)
              (length args)
        )
      elabArgs <- zipWithM checkApplyArg tArgs args -- check each arg against type
      unified <- gets tcsUnified
      actualTyReturn <-
        checkReturnType
          tReturn
          (substitute unified tReturn)
      pure (actualTyReturn, elabArgs)
    _ -> throwError $ NonFunctionTypeFound ann fn

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

infer :: Expr ann -> TypecheckM ann (Expr (Type ann))
infer (EPrim ann prim) =
  pure (EPrim (typeFromPrim ann prim) prim)
infer (EBox ann inner) = do
  typedInner <- infer inner
  pure $
    EBox
      ( TContainer
          ann
          (NE.singleton $ getOuterAnnotation typedInner)
      )
      typedInner
infer (ELet ann pat expr rest) = do
  typedExpr <- infer expr
  typedPat <- checkPattern (getOuterAnnotation typedExpr) pat
  typedRest <- withVar pat (getOuterAnnotation typedExpr) (infer rest)
  pure $ ELet (getOuterAnnotation typedRest $> ann) typedPat typedExpr typedRest
infer (EIf ann predExpr thenExpr elseExpr) =
  inferIf ann predExpr thenExpr elseExpr
infer (ETuple ann fstExpr restExpr) = do
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
infer (EContainerAccess ann tup index) = do
  tyTup <- infer tup
  case getOuterAnnotation tyTup of
    TContainer _ tyAll ->
      let tyNumbered = zip ([0 ..] :: [Int]) (NE.toList tyAll)
       in case List.lookup (fromIntegral $ index - 1) tyNumbered of
            Just ty ->
              pure (EContainerAccess ty tyTup index)
            Nothing -> throwError $ AccessingOutsideTupleBounds ann (getOuterAnnotation tyTup) index
    otherTy -> throwError $ AccessingNonTuple ann otherTy
infer (EApply ann fnName args) =
  inferApply ann fnName args
infer (EVar ann var) = do
  ty <- lookupVar ann var
  pure (EVar (ty $> ann) var)
infer (EInfix ann op a b) =
  inferInfix ann op a b

typePrimFromPrim :: Prim -> TypePrim
typePrimFromPrim (PBool _) = TBool
typePrimFromPrim (PInt32 _) = TInt32
typePrimFromPrim (PInt64 _) = TInt64
typePrimFromPrim (PFloat32 _) = TFloat32
typePrimFromPrim (PFloat64 _) = TFloat64

typeFromPrim :: ann -> Prim -> Type ann
typeFromPrim ann prim = TPrim ann (typePrimFromPrim prim)
