{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.FromExpr (fromModule) where

import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Module
import Calc.Types.Type
import Calc.Wasm.Types
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import GHC.Natural

-- | take our regular module and do the book keeping to get it ready for Wasm
-- town
data FromWasmError
  = FunctionTypeNotScalar
  | IdentifierNotFound Identifier
  | FunctionNotFound FunctionName
  deriving stock (Eq, Ord, Show)

data FromExprEnv = FromExprEnv
  { feeIdentifiers :: M.Map Identifier Natural,
    feeFunctions :: M.Map FunctionName Natural
  }

lookupIdent ::
  (MonadReader FromExprEnv m, MonadError FromWasmError m) =>
  Identifier ->
  m Natural
lookupIdent ident = do
  maybeNat <- asks (M.lookup ident . feeIdentifiers)
  case maybeNat of
    Just nat -> pure nat
    Nothing -> throwError $ IdentifierNotFound ident

lookupFunction ::
  (MonadReader FromExprEnv m, MonadError FromWasmError m) =>
  FunctionName ->
  m Natural
lookupFunction functionName = do
  maybeNat <- asks (M.lookup functionName . feeFunctions)
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
    MonadReader FromExprEnv m,
    Show ann
  ) =>
  Expr ann ->
  m WasmExpr
fromExpr (EPrim _ prim) = pure $ WPrim prim
fromExpr (EInfix _ op a b) = WInfix op <$> fromExpr a <*> fromExpr b
fromExpr (EIf _ predE thenE elseE) =
  WIf <$> fromExpr predE <*> fromExpr thenE <*> fromExpr elseE
fromExpr (EVar _ ident) = WVar <$> lookupIdent ident
fromExpr (EApply _ funcName args) =
  WApply <$> lookupFunction funcName <*> (traverse fromExpr args) -- need to look up the function name in some sort of state
fromExpr other = error $ "fromExpr error: " <> show other

fromFunction :: (Show ann) =>
  M.Map FunctionName Natural -> Function ann -> Either FromWasmError WasmFunction
fromFunction funcMap (Function {fnBody, fnArgs, fnFunctionName}) = do
  args <- traverse (scalarFromType . snd) fnArgs
  let argMap = M.fromList $ (\(i, (ArgumentName ident, _)) -> (Identifier ident, i)) <$> zip [0 ..] fnArgs
  expr <- runReaderT (fromExpr fnBody) (FromExprEnv argMap funcMap)
  pure $
    WasmFunction
      { wfName = fnFunctionName,
        wfExpr = expr,
        wfPublic = False,
        wfArgs = args,
        wfReturnType = I32 -- a pure guess, we should use the typed module here and grab the type from `ann`
      }

fromModule :: (Show ann) => Module ann -> Either FromWasmError WasmModule
fromModule (Module {mdExpr, mdFunctions}) = do
  let funcMap = M.fromList $ (\(i, Function {fnFunctionName}) -> (fnFunctionName, i + 1)) <$> zip [0 ..] mdFunctions

  expr <- runReaderT (fromExpr mdExpr) (FromExprEnv mempty funcMap)

  let mainFunction =
        WasmFunction
          { wfName = "main",
            wfExpr = expr,
            wfPublic = True,
            wfArgs = mempty,
            wfReturnType = I32
          }

  wasmFunctions <- traverse (fromFunction funcMap) mdFunctions
  pure $
    WasmModule
      { wmFunctions = mainFunction : wasmFunctions
      }
