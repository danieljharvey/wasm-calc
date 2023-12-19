{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calc.Typecheck.Elaborate
  ( elaborate,
    elaborateFunction,
    elaborateModule,
  )
where

import           Calc.ExprUtils
import           Calc.Typecheck.Error
import           Calc.Typecheck.Types
import           Calc.Types.Expr
import           Calc.Types.Function
import           Calc.Types.Module
import           Calc.Types.Prim
import           Calc.Types.Type
import           Calc.TypeUtils
import           Control.Monad        (when, zipWithM)
import           Control.Monad.Except
import           Data.Bifunctor       (second)
import           Data.Functor
import qualified Data.List            as List
import qualified Data.List.NonEmpty   as NE
import qualified Data.Set             as S

elaborateModule ::
  forall ann.
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule (Module {mdFunctions, mdExpr }) = runTypecheckM (TypecheckEnv mempty mempty) $ do
  fns <-
    traverse
      ( \fn -> do
          elabFn <- elaborateFunction fn
          storeFunction (fnFunctionName elabFn) (S.fromList $ fnGenerics fn) (fnAnn elabFn)
          pure elabFn
      )
      mdFunctions

  Module fns <$> infer mdExpr

elaborateFunction ::
  Function ann ->
  TypecheckM ann (Function (Type ann))
elaborateFunction (Function {fnAnn, fnArgs, fnGenerics, fnFunctionName, fnBody}) = do
  exprA <- withFunctionEnv fnArgs (S.fromList fnGenerics) (infer fnBody)
  let argsA = fmap (second (\ty -> fmap (const ty) ty)) fnArgs
  let tyFn = TFunction fnAnn (snd <$> fnArgs) (getOuterAnnotation exprA)
  pure
    ( Function
        { fnAnn = tyFn,
          fnGenerics,
          fnArgs = argsA,
          fnFunctionName = fnFunctionName,
          fnBody = exprA
        }
    )

elaborate :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
elaborate = runTypecheckM (TypecheckEnv mempty mempty) . infer

check :: Type ann -> Expr ann -> TypecheckM ann (Expr (Type ann))
check ty expr = do
  exprA <- infer expr
  _ <- checkTypeIsEqual ty (getOuterAnnotation exprA)
  pure (mapOuterExprAnnotation (const ty) exprA)

-- simple check for now
checkTypeIsEqual :: Type ann -> Type ann -> TypecheckM ann (Type ann)
checkTypeIsEqual tyA tyB =
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

inferInfix ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
inferInfix ann OpEquals a b = do
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
  pure (EInfix ty OpEquals elabA elabB)
inferInfix ann op a b = do
  elabA <- infer a
  elabB <- infer b
  -- all the other infix operators need to be Int -> Int -> Int
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ TInt, TPrim _ TInt) ->
      -- if the types are the same, then great! it's an int!
      pure (TPrim ann TInt)
    (TPrim _ TFloat, TPrim _ TFloat) ->
      -- if the types are the same, then great! it's a float!
      pure (TPrim ann TFloat)
    (otherA, otherB) ->
      let filterSame (tA, tB) = void tA /= void tB
       in -- otherwise, error!
          throwError
            ( InfixTypeMismatch
                op
                $ filter
                  filterSame
                  [ (TPrim (getOuterTypeAnnotation otherA) TInt, otherA),
                    (TPrim (getOuterTypeAnnotation otherB) TInt, otherB)
                  ]
            )
  pure (EInfix ty op elabA elabB)

inferApply :: ann -> FunctionName -> [Expr ann] ->
    TypecheckM ann (Expr (Type ann))
inferApply ann fnName args = do
  fn <- lookupFunction ann fnName
  (ty, elabArgs) <- case fn of
    TFunction _ tArgs tReturn -> do
      when
        (length args /= length tArgs)
        (throwError $ FunctionArgumentLengthMismatch ann (length tArgs) (length args))
      elabArgs <- zipWithM check tArgs args -- check each arg against type
      pure (tReturn, elabArgs)
    _ -> throwError $ NonFunctionTypeFound ann fn
  pure (EApply (ty $> ann) fnName elabArgs)

infer :: Expr ann -> TypecheckM ann (Expr (Type ann))
infer (EPrim ann prim) =
  pure (EPrim (typeFromPrim ann prim) prim)
infer (EIf ann predExpr thenExpr elseExpr) =
  inferIf ann predExpr thenExpr elseExpr
infer (ETuple ann fstExpr restExpr) = do
  typedFst <- infer fstExpr
  typedRest <- traverse infer restExpr
  let typ =
        TTuple
          ann
          (getOuterAnnotation typedFst)
          (getOuterAnnotation <$> typedRest)
  pure $ ETuple typ typedFst typedRest
infer (ETupleAccess ann tup index) = do
  tyTup <- infer tup
  case getOuterAnnotation tyTup of
    TTuple _ tyFst tyRest ->
      let tyAll = zip ([0 ..] :: [Int]) (tyFst : NE.toList tyRest)
       in case List.lookup (fromIntegral $ index - 1) tyAll of
            Just ty ->
              pure (ETupleAccess ty tyTup index)
            Nothing -> throwError $ AccessingOutsideTupleBounds ann (getOuterAnnotation tyTup) index
    otherTy -> throwError $ AccessingNonTuple ann otherTy
infer (EApply ann fnName args) =
  inferApply ann fnName args
infer (EVar ann var) = do
  ty <- lookupVar ann var
  pure (EVar ty var)
infer (EInfix ann op a b) =
  inferInfix ann op a b

typePrimFromPrim :: Prim -> TypePrim
typePrimFromPrim (PInt _)   = TInt
typePrimFromPrim (PBool _)  = TBool
typePrimFromPrim (PFloat _) = TFloat

typeFromPrim :: ann -> Prim -> Type ann
typeFromPrim ann prim = TPrim ann (typePrimFromPrim prim)
