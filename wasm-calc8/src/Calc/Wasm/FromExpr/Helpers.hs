{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Wasm.FromExpr.Helpers
  ( functionOffset,
    scalarFromType,
    getAllocationFunctionNumber,
    addLocal,
    lookupGlobal,
    lookupIdent,
    getGlobalMap,
    getFunctionMap,
    getImportMap,
    lookupFunction,
  )
where

import Calc.ExprUtils
import Calc.Types
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad.Except
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map.Strict as M
import GHC.Natural

-- when should user-specified functions begin? after all the allocator
-- functions!
functionOffset :: Natural
functionOffset = 1

-- | add a local type, returning a unique index
addLocal ::
  (MonadState FromExprState m) =>
  Maybe Identifier ->
  WasmType ->
  m Natural
addLocal maybeIdent ty = do
  modify
    ( \fes ->
        fes
          { fesVars =
              fesVars fes <> [(maybeIdent, ty)]
          }
    )

  varLen <-
    gets
      ( fromIntegral . length . fesVars
      )

  argLen <-
    gets (fromIntegral . length . fesArgs)

  pure (argLen + varLen - 1)

lookupGlobal ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m
  ) =>
  Identifier ->
  m Natural
lookupGlobal ident = do
  maybeGlobalNat <-
    gets
      ( M.lookup ident
          . fesGlobals
      )

  case maybeGlobalNat of
    Just (FromExprGlobal {fegIndex}) -> pure fegIndex
    Nothing ->
      throwError $ IdentifierNotFound ident

lookupIdent ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  Identifier ->
  m Natural
lookupIdent ident = do
  let matchVarIdent (_, (thisIdent, _)) = thisIdent == Just ident
      matchArgIdent (_, (thisIdent, _)) = thisIdent == ident

  startingDigit <- gets (fromIntegral . length . fesArgs)

  maybeVarNat <-
    gets
      ( List.find matchVarIdent
          . zip [startingDigit ..]
          . fesVars
      )
  case maybeVarNat of
    Just (nat, _) -> pure nat
    Nothing -> do
      -- check in args
      maybeArgNat <-
        gets
          ( List.find matchArgIdent
              . zip [0 ..]
              . fesArgs
          )
      case maybeArgNat of
        Just (nat, _) -> pure nat
        Nothing ->
          throwError $ IdentifierNotFound ident

-- | user defined functions live after any imports, and our alloc is the second
-- function
getAllocationFunctionNumber :: (MonadState FromExprState m) => m Natural
getAllocationFunctionNumber = do
  gets (fromIntegral . length . fesImports)

lookupFunction ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  FunctionName ->
  m Natural
lookupFunction functionName = do
  maybeFunc <- gets (M.lookup functionName . fesFunctions)
  case maybeFunc of
    Just (FromExprFunc {fefIndex}) -> pure (fefIndex + functionOffset)
    Nothing -> do
      maybeImport <- gets (M.lookup functionName . fesImports)
      case maybeImport of
        Just (FromExprImport {feiIndex}) -> pure feiIndex
        Nothing ->
          throwError $ FunctionNotFound functionName

-- take only the information about globals that we need
-- we assume each global uses no imports or functions
getGlobalMap ::
  [Global (Type ann)] ->
  Either FromWasmError (M.Map Identifier FromExprGlobal)
getGlobalMap globals =
  M.fromList
    <$> traverse
      ( \(fegIndex, Global {glbIdentifier}) -> do
          pure (glbIdentifier, FromExprGlobal {fegIndex})
      )
      (zip [0 ..] globals)

-- take only the function info we need
getFunctionMap ::
  Natural ->
  [Function (Type ann)] ->
  Either
    FromWasmError
    (M.Map FunctionName FromExprFunc)
getFunctionMap offset mdFunctions =
  M.fromList
    <$> traverse
      ( \(i, Function {fnFunctionName, fnArgs, fnBody}) -> do
          fefArgs <- traverse (scalarFromType . faType) fnArgs
          fefReturnType <- scalarFromType (getOuterAnnotation fnBody)
          pure
            ( fnFunctionName,
              FromExprFunc {fefIndex = i, fefArgs, fefReturnType}
            )
      )
      (zip [offset ..] mdFunctions)

-- take only the function info we need
getImportMap ::
  [Import (Type ann)] ->
  Either
    FromWasmError
    (M.Map FunctionName FromExprImport)
getImportMap mdImports =
  M.fromList
    <$> traverse
      ( \(i, Import {impImportName}) -> do
          pure
            ( impImportName,
              FromExprImport {feiIndex = i}
            )
      )
      (zip [0 ..] mdImports)

scalarFromType :: Type ann -> Either FromWasmError WasmType
scalarFromType (TPrim _ TVoid) = pure Void
scalarFromType (TPrim _ TBool) = pure I32
scalarFromType (TPrim _ TInt8) = pure I8
scalarFromType (TPrim _ TInt16) = pure I16
scalarFromType (TPrim _ TInt32) = pure I32
scalarFromType (TPrim _ TInt64) = pure I64
scalarFromType (TPrim _ TFloat32) = pure F32
scalarFromType (TPrim _ TFloat64) = pure F64
scalarFromType (TFunction {}) = Left FunctionTypeNotScalar
scalarFromType (TContainer {}) = pure Pointer
scalarFromType (TVar _ _) =
  pure Pointer -- all polymorphic variables are Pointer
scalarFromType (TUnificationVar {}) =
  pure Pointer
