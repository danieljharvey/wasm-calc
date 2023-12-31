{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.FromExpr (fromModule) where

import Calc.ExprUtils
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Module
import Calc.Types.Type
import Calc.Wasm.Helpers
import Calc.Wasm.Types
import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Monoid
import GHC.Natural

-- | take our regular module and do the book keeping to get it ready for Wasm
-- town
data FromWasmError
  = FunctionTypeNotScalar
  | IdentifierNotFound Identifier
  | FunctionNotFound FunctionName
  deriving stock (Eq, Ord, Show)

data FromExprState = FromExprState
  { fesIdentifiers :: M.Map Identifier Natural,
    fesFunctions :: M.Map FunctionName FromExprFunc,
    fesItems :: [WasmType]
  }

data FromExprFunc = FromExprFunc
  { fefIndex :: Natural,
    fefArgs :: [WasmType],
    fefReturnType :: WasmType
  }

addLocal ::
  (MonadState FromExprState m) =>
  Maybe Identifier ->
  WasmType ->
  m Natural
addLocal maybeIdent ty = do
  len <- gets (fromIntegral . (\fes -> length (fesIdentifiers fes) + length (fesItems fes)))
  modify (\fes -> fes {fesItems = fesItems fes <> [ty]})
  case maybeIdent of
    Just ident -> modify (\fes -> fes {fesIdentifiers = fesIdentifiers fes <> M.singleton ident len})
    Nothing -> pure ()
  pure len

lookupIdent ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  Identifier ->
  m Natural
lookupIdent ident = do
  maybeNat <- gets (M.lookup ident . fesIdentifiers)
  case maybeNat of
    Just nat -> pure nat
    Nothing -> throwError $ IdentifierNotFound ident

lookupFunction ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  FunctionName ->
  m FromExprFunc
lookupFunction functionName = do
  maybeNat <- gets (M.lookup functionName . fesFunctions)
  case maybeNat of
    Just nat -> pure nat
    Nothing -> throwError $ FunctionNotFound functionName

scalarFromType :: Type ann -> Either FromWasmError WasmType
scalarFromType (TPrim _ TInt) = pure I64
scalarFromType (TPrim _ TBool) = pure I32
scalarFromType (TPrim _ TFloat) = pure F64
scalarFromType (TFunction {}) = Left FunctionTypeNotScalar
scalarFromType (TContainer {}) = pure Pointer
scalarFromType (TVar _ _) =
  pure Pointer -- all polymorphic variables are Pointer
scalarFromType (TUnificationVar {}) =
  error "scalarFromType TUnificationVar"

fromExpr ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann
  ) =>
  Expr (Type ann) ->
  m WasmExpr
fromExpr (EPrim _ prim) = do
  pure (WPrim prim)
fromExpr (EInfix _ op a b) = do
  -- we're assuming that the types of `a` and `b` are the same
  -- we want the type of the args, not the result
  scalar <- liftEither $ scalarFromType (getOuterAnnotation a)
  WInfix scalar op <$> fromExpr a <*> fromExpr b
fromExpr (EIf _ predE thenE elseE) =
  WIf <$> fromExpr predE <*> fromExpr thenE <*> fromExpr elseE
fromExpr (EVar _ ident) =
  WVar <$> lookupIdent ident
fromExpr (EApply _ funcName args) = do
  (FromExprFunc {fefIndex}) <- lookupFunction funcName
  WApply fefIndex
    <$> traverse fromExpr args
fromExpr (ETuple ty a as) = do
  wasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing wasmType
  let allItems = zip [0 ..] (a : NE.toList as)
      tupleLength = memorySizeForType ty
      allocate = WAllocate (fromIntegral tupleLength)
      offsetList = getOffsetList ty
  WSet index allocate
    <$> traverse
      ( \(i, item) ->
          (,,) (offsetList !! i)
            <$> liftEither (scalarFromType (getOuterAnnotation item))
            <*> fromExpr item
      )
      allItems
fromExpr (EContainerAccess ty tup nat) =
  let offset = getOffsetList (getOuterAnnotation tup) !! fromIntegral (nat - 1)
   in WTupleAccess
        <$> liftEither (scalarFromType ty)
        <*> fromExpr tup
        <*> pure offset
fromExpr (EBox ty inner) = do
  innerWasmType <- liftEither $ scalarFromType (getOuterAnnotation inner)
  containerWasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing containerWasmType
  boxed index innerWasmType <$> fromExpr inner

-- | wrap a `WasmExpr` in a single item struct
boxed :: Natural -> WasmType -> WasmExpr -> WasmExpr
boxed index ty wExpr =
  let allocate = WAllocate (memorySize ty)
   in WSet index allocate [(0, ty, wExpr)]

getOffsetList :: Type ann -> [Natural]
getOffsetList (TContainer _ items) =
  scanl (\offset item -> offset + offsetForType item) 0 (NE.toList items)
getOffsetList _ = []

-- | size of the primitive in memory (ie, struct is size of its pointer)
offsetForType :: Type ann -> Natural
offsetForType (TPrim _ TInt) =
  memorySize I64
offsetForType (TPrim _ TFloat) =
  memorySize F64
offsetForType (TPrim _ TBool) =
  memorySize I32
offsetForType (TContainer _ _) =
  memorySize Pointer
offsetForType (TFunction {}) =
  memorySize Pointer
offsetForType (TVar _ _) =
  memorySize Pointer
offsetForType (TUnificationVar _ _) =
  error "offsetForType TUnificationVar"

-- | the actual size of the item in memory
memorySizeForType :: Type ann -> Natural
memorySizeForType (TPrim _ TInt) =
  memorySize I64
memorySizeForType (TPrim _ TFloat) =
  memorySize F64
memorySizeForType (TPrim _ TBool) =
  memorySize I32
memorySizeForType (TContainer _ as) =
  getSum (foldMap (Sum . memorySizeForType) as)
memorySizeForType (TFunction {}) =
  memorySize Pointer
memorySizeForType (TVar _ _) =
  memorySize Pointer
memorySizeForType (TUnificationVar _ _) =
  error "memorySizeForType TUnificationVar"

fromFunction ::
  (Show ann) =>
  M.Map FunctionName FromExprFunc ->
  Function (Type ann) ->
  Either FromWasmError WasmFunction
fromFunction funcMap (Function {fnBody, fnArgs, fnFunctionName}) = do
  args <- traverse (scalarFromType . snd) fnArgs
  let argMap =
        M.fromList $
          ( \(i, (ArgumentName ident, _)) ->
              (Identifier ident, i)
          )
            <$> zip [0 ..] fnArgs

  (expr, fes) <- runStateT (fromExpr fnBody) (FromExprState argMap funcMap mempty)

  retType <- scalarFromType (getOuterAnnotation fnBody)

  pure $
    WasmFunction
      { wfName = fnFunctionName,
        wfExpr = expr,
        wfPublic = False,
        wfArgs = args,
        wfReturnType = retType,
        wfLocals = fesItems fes
      }

-- take only the function info we need
getFunctionMap :: [Function (Type ann)] -> Either FromWasmError (M.Map FunctionName FromExprFunc)
getFunctionMap mdFunctions =
  M.fromList
    <$> traverse
      ( \(i, Function {fnFunctionName, fnArgs, fnBody}) -> do
          fefArgs <- traverse (scalarFromType . snd) fnArgs
          fefReturnType <- scalarFromType (getOuterAnnotation fnBody)
          pure
            ( fnFunctionName,
              FromExprFunc {fefIndex = i + 1, fefArgs, fefReturnType}
            )
      )
      (zip [0 ..] mdFunctions)

fromModule ::
  (Show ann) =>
  Module (Type ann) ->
  Either FromWasmError WasmModule
fromModule (Module {mdExpr, mdFunctions}) = do
  funcMap <- getFunctionMap mdFunctions

  (expr, fes) <-
    runStateT
      (fromExpr mdExpr)
      (FromExprState mempty funcMap mempty)

  retType <- scalarFromType (getOuterAnnotation mdExpr)

  let mainFunction =
        WasmFunction
          { wfName = "main",
            wfExpr = expr,
            wfPublic = True,
            wfArgs = mempty,
            wfReturnType = retType,
            wfLocals = fesItems fes
          }

  wasmFunctions <- traverse (fromFunction funcMap) mdFunctions
  pure $
    WasmModule
      { wmFunctions = mainFunction : wasmFunctions
      }
