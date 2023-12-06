{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.FromExpr (fromModule) where

import Control.Monad (void)
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
    fesFunctions :: M.Map FunctionName Natural,
    fesItems :: [WasmType]
  }

addLocal ::
  (MonadState FromExprState m) =>
  Maybe Identifier ->
  WasmType ->
  m Natural
addLocal maybeIdent ty = do
  len <- fromIntegral <$> gets (\fes -> length (fesIdentifiers fes) + length (fesItems fes))
  modify (\fes -> fes {fesItems = fesItems fes <> [ty]})
  case maybeIdent of
    Just ident -> modify (\fes -> fes { fesIdentifiers = fesIdentifiers fes <> M.singleton ident len })
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
  m Natural
lookupFunction functionName = do
  maybeNat <- gets (M.lookup functionName . fesFunctions)
  case maybeNat of
    Just nat -> pure nat
    Nothing -> throwError $ FunctionNotFound functionName

scalarFromType :: Type ann -> Either FromWasmError WasmType
scalarFromType (TPrim _ TInt) = pure I32
scalarFromType (TPrim _ TBool) = pure I32
scalarFromType (TFunction {}) = Left FunctionTypeNotScalar
scalarFromType (TTuple {}) = pure Pointer

fromExpr ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann
  ) =>
  Expr (Type ann) ->
  m WasmExpr
fromExpr (EPrim _ prim) =
  pure $ WPrim prim
fromExpr (EInfix _ op a b) =
  WInfix op <$> fromExpr a <*> fromExpr b
fromExpr (EIf _ predE thenE elseE) =
  WIf <$> fromExpr predE <*> fromExpr thenE <*> fromExpr elseE
fromExpr (EVar _ ident) =
  WVar <$> lookupIdent ident
fromExpr (EApply _ funcName args) =
  WApply <$> lookupFunction funcName <*> traverse fromExpr args -- need to look up the function name in some sort of state
fromExpr (ETuple ty a as) = do
  wasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing wasmType
  let allItems = zip [0 ..] (a : NE.toList as)
      tupleLength = memorySizeForType ty
      allocate = WAllocate (fromIntegral tupleLength)
  WSet index allocate
    <$> traverse
      ( \(i, item) ->
          (,) i <$> fromExpr item
      )
      allItems
fromExpr (EPatternMatch _ matchExpr pats) = do
  wasmMatch <- fromExpr matchExpr
  -- need to get items from `pat` and put them in scope
  let fromPat (pat,expr) =
          (,) (void pat) <$> fromExpr expr 
  WPatternMatch wasmMatch <$> traverse fromPat pats

memorySizeForType :: Type ann -> Natural
memorySizeForType (TPrim _ TInt) =
  memorySize I32
memorySizeForType (TPrim _ TBool) =
  memorySize I32
memorySizeForType (TTuple _ a as) =
  memorySizeForType a + getSum (foldMap (Sum . memorySizeForType) as)
memorySizeForType (TFunction {}) =
  memorySize Pointer

fromFunction ::
  (Show ann) =>
  M.Map FunctionName Natural ->
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

  pure $
    WasmFunction
      { wfName = fnFunctionName,
        wfExpr = expr,
        wfPublic = False,
        wfArgs = args,
        wfReturnType = I32, -- a pure guess, we should use the typed module here and grab the type from `ann`
        wfLocals = fesItems fes
      }

fromModule :: (Show ann) => Module (Type ann) -> Either FromWasmError WasmModule
fromModule (Module {mdExpr, mdFunctions}) = do
  let funcMap =
        M.fromList $
          ( \(i, Function {fnFunctionName}) ->
              (fnFunctionName, i + 1)
          )
            <$> zip [0 ..] mdFunctions

  (expr, fes) <- runStateT (fromExpr mdExpr) (FromExprState mempty funcMap mempty)

  let mainFunction =
        WasmFunction
          { wfName = "main",
            wfExpr = expr,
            wfPublic = True,
            wfArgs = mempty,
            wfReturnType = I32,
            wfLocals = fesItems fes
          }

  wasmFunctions <- traverse (fromFunction funcMap) mdFunctions
  pure $
    WasmModule
      { wmFunctions = mainFunction : wasmFunctions
      }
