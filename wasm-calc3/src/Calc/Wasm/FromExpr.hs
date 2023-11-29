{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
  {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE FlexibleContexts #-}
module Calc.Wasm.FromExpr (fromModule) where

import Debug.Trace
import Control.Monad.Except
import Calc.Types.Function
import Calc.Types.Module
import Calc.Wasm.Types
import Calc.Types.Expr
import Calc.Types.Type
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Calc.Types.Identifier
import GHC.Natural

-- | take our regular module and do the book keeping to get it ready for Wasm
-- town
data FromWasmError = FunctionTypeNotScalar
  | IdentifierNotFound Identifier | FunctionNotFound FunctionName
  deriving stock (Eq, Ord, Show)

data FromExprEnv = FromExprEnv {
  feeIdentifiers :: M.Map Identifier Natural,
  feeFunctions :: M.Map FunctionName Natural
                                  }

lookupIdent :: (MonadReader FromExprEnv m, MonadError FromWasmError m) => 
  Identifier -> m Natural
lookupIdent ident = do
  maybeNat <- asks (M.lookup ident . feeIdentifiers)
  case maybeNat of
    Just nat -> pure nat
    Nothing -> throwError $ IdentifierNotFound ident

lookupFunction :: (MonadReader FromExprEnv m, MonadError FromWasmError m) => 
  FunctionName -> m Natural
lookupFunction functionName = do
  maybeNat <- asks (M.lookup functionName . feeFunctions)
  case maybeNat of
    Just nat -> pure nat
    Nothing -> throwError $ FunctionNotFound functionName

fromType :: Type ann -> Either FromWasmError WasmType
fromType (TPrim _ TInt) = pure I32
fromType (TPrim _ TBool) = pure I32
fromType (TFunction {}) = Left FunctionTypeNotScalar

fromExpr :: (MonadError FromWasmError m,
  MonadReader FromExprEnv m) => Expr ann -> m WasmExpr
fromExpr (EPrim _ prim) = pure $ WPrim prim
fromExpr (EInfix _ op a b) = WInfix op <$> fromExpr a <*> fromExpr b
fromExpr (EIf _ predE thenE elseE)
  = WIf <$> fromExpr predE <*> fromExpr thenE <*> fromExpr elseE
fromExpr (EVar _ ident ) = WVar <$> lookupIdent ident
fromExpr (EApply _ funcName args)
  = WApply <$> lookupFunction funcName <*> (traverse fromExpr args) -- need to look up the function name in some sort of state

fromFunction :: M.Map FunctionName Natural -> Function ann -> Either FromWasmError WasmFunction 
fromFunction funcMap (Function {fnBody,fnArgs,fnFunctionName}) = do
  args <- traverse (fromType . snd) fnArgs
  let argMap = M.fromList $ (\(i,(ArgumentName ident,_)) -> (Identifier ident,i + 1)) <$> zip [0..] fnArgs
  expr <- runReaderT (fromExpr fnBody) (FromExprEnv argMap funcMap)
  pure $
    WasmFunction
      { wfName = fnFunctionName, 
        wfExpr = expr, 
        wfPublic = False,
        wfArgs = args, 
        wfReturnType = I32 -- a pure guess, we should use the typed module here and grab the type from `ann`
      }

fromModule :: Module ann -> Either FromWasmError WasmModule 
fromModule (Module {mdExpr, mdFunctions}) = do
  let funcMap = M.fromList $ (\(i,Function {fnFunctionName}) -> (fnFunctionName,i + 1)) <$> zip [0..] mdFunctions
 
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
  pure $ traceShowId $ 
    WasmModule
      { wmFunctions = mainFunction : wasmFunctions
      }
