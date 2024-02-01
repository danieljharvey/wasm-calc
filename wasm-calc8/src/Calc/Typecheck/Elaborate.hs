{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calc.Typecheck.Elaborate
  ( infer,
    elaborateFunction,
    elaborateModule,
  )
where

import           Calc.ExprUtils
import           Calc.Typecheck.Error
import           Calc.Typecheck.Helpers
import           Calc.Typecheck.Substitute
import           Calc.Typecheck.Types
import           Calc.Types.Expr
import           Calc.Types.Function
import           Calc.Types.Import
import           Calc.Types.Module
import           Calc.Types.Op
import           Calc.Types.Pattern
import           Calc.Types.Prim
import           Calc.Types.Type
import           Calc.TypeUtils
import           Control.Monad             (when, zipWithM)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor
import qualified Data.List                 as List
import qualified Data.List.NonEmpty        as NE
import qualified Data.Set                  as S

elaborateModule ::
  forall ann.
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule (Module {mdImports, mdMemory, mdFunctions}) =
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
          mdImports = imports,
          mdMemory
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

checkAndSubstitute ::
  Type ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
checkAndSubstitute ty expr = do
  exprA <- check ty expr
  unified <- gets tcsUnified
  pure $ substitute unified <$> exprA

elaborateFunction ::
  Function ann ->
  TypecheckM ann (Function (Type ann))
elaborateFunction (Function {fnPublic, fnAnn, fnArgs, fnGenerics, fnReturnType, fnFunctionName, fnBody}) = do
  exprA <-
    withFunctionEnv
      fnArgs
      (S.fromList fnGenerics)
      (checkAndSubstitute fnReturnType fnBody)
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
          fnPublic = fnPublic,
          fnReturnType = fnReturnType $> fnReturnType
        }
    )

check :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
check ty (EApply ann fn args) =
  checkApply (Just ty) ann fn args
check ty (EInfix ann op a b) =
  checkInfix (Just ty) ann op a b
check ty (ETuple ann fstExpr restExpr) =
  checkTuple (Just ty) ann fstExpr restExpr
check (TPrim ann tyPrim) (EPrim _ (PIntLit i)) = do
  let ty = TPrim ann tyPrim
  pure $ EPrim ty (PIntLit i)
check (TContainer tyAnn tyItems) (EBox _ inner) | length tyItems == 1 = do
  typedInner <- check (NE.head tyItems) inner
  let ty = TContainer tyAnn (NE.singleton (getOuterAnnotation typedInner))
  pure $ EBox ty typedInner
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
    otherType       -> throwError (PredicateIsNotBoolean ann otherType)
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
checkInfix (Just ty) ann op a b = do
  elabA <- check ty a
  elabB <- check ty b
  pure (EInfix (ty $> ann) op elabA elabB)
checkInfix Nothing ann op a b = do
  elabA <- infer a
  elabB <- infer b
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ TInt32, TPrim _ TInt32) ->
      -- if the types are the same, then great! it's an int32!
      pure (TPrim ann TInt32)
    (TPrim _ TInt64, TPrim _ TInt64) ->
      -- if the types are the same, then great! it's an int64!
      pure (TPrim ann TInt64)
    (TPrim _ TFloat32, TPrim _ TFloat32) ->
      -- if the types are the same, then great! it's a float32!
      pure (TPrim ann TFloat32)
    (TPrim _ TFloat64, TPrim _ TFloat64) ->
      -- if the types are the same, then great! it's a float64!
      pure (TPrim ann TFloat64)
    (otherA, otherB) -> throwError (InfixTypeMismatch op otherA otherB)
  pure (EInfix ty op elabA elabB)

-- | like `check`, but we also check we're not passing a non-boxed value to a
-- generic argument
checkApplyArg :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
checkApplyArg ty@(TUnificationVar {}) expr = do
  tyExpr <- check ty expr
  case getOuterAnnotation tyExpr of
    p@TPrim {} -> throwError (NonBoxedGenericValue (getOuterTypeAnnotation p) p)
    _other     -> pure tyExpr
checkApplyArg ty expr = check ty expr

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
    TFunction _ fnArgs fnReturn -> do
      when
        (length args /= length fnArgs)
        ( throwError $
            FunctionArgumentLengthMismatch
              ann
              (length fnArgs)
              (length args)
        )

      -- what do we learn about args from return type?
      _ <- case maybeTy of
        Just ty -> void (unify ty fnReturn)
        Nothing -> pure ()

      unified <- gets tcsUnified

      -- what have we learned?
      let substitutedArgs = substitute unified <$> fnArgs

      elabArgs <- zipWithM checkApplyArg substitutedArgs args -- check each arg against type

      -- did we learn yet more?
      moreUnified <- gets tcsUnified

      actualTyReturn <-
        checkReturnType
          fnReturn
          (substitute moreUnified fnReturn)
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

infer :: Expr ann -> TypecheckM ann (Expr (Type ann))
infer (EAnn ann ty expr) = do
  typedExpr <- check ty expr
  pure $ EAnn (getOuterAnnotation typedExpr $> ann) (ty $> ty) typedExpr
infer (EPrim ann prim) =
  case typeFromPrim ann prim of
    Just ty -> pure (EPrim ty prim)
    Nothing -> case prim of
      -- ints default to Int64
      PIntLit _ -> pure (EPrim (TPrim ann TInt64) prim)
      _         -> error "don't know what int type to use"
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
infer (ETuple ann fstExpr restExpr) =
  checkTuple Nothing ann fstExpr restExpr
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
  checkApply Nothing ann fnName args
infer (EVar ann var) = do
  ty <- lookupVar ann var
  pure (EVar (ty $> ann) var)
infer (EInfix ann op a b) =
  checkInfix Nothing ann op a b

typePrimFromPrim :: Prim -> Maybe TypePrim
typePrimFromPrim (PBool _)    = pure TBool
typePrimFromPrim (PIntLit _)  = Nothing
typePrimFromPrim (PFloat32 _) = pure TFloat32
typePrimFromPrim (PFloat64 _) = pure TFloat64

typeFromPrim :: ann -> Prim -> Maybe (Type ann)
typeFromPrim ann prim = TPrim ann <$> typePrimFromPrim prim
