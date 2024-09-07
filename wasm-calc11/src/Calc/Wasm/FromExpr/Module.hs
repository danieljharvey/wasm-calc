{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Calc.Wasm.FromExpr.Module (fromModule) where

import Calc.Ability.Check
import Calc.ExprUtils
import Calc.Linearity (getFunctionUses)
import Calc.Types
import Calc.Wasm.FromExpr.Expr
import Calc.Wasm.FromExpr.Helpers
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad (foldM, void)
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S

fromImport :: Import (Type ann) -> Either FromWasmError WasmImport
fromImport
  ( Import
      { impReturnType,
        impExternalModule = Identifier wiExternalModule,
        impExternalFunction = Identifier wiExternalFunction,
        impImportName,
        impArgs
      }
    ) = do
    args <-
      traverse
        ( \(ImportArg {iaName = ident, iaType}) -> do
            wasmType <- scalarFromType iaType
            pure (Just ident, wasmType)
        )
        impArgs

    wiReturnType <- scalarFromType impReturnType

    pure $
      WasmImport
        { wiName = impImportName,
          wiArgs = snd <$> args,
          wiExternalModule,
          wiExternalFunction,
          wiReturnType
        }

-- | tests don't use imports
fromTest ::
  (Eq ann, Show ann) =>
  M.Map FunctionName FromExprFunc ->
  M.Map Identifier FromExprGlobal ->
  M.Map DataName (Data ()) ->
  Test (Type ann) ->
  Either FromWasmError WasmTest
fromTest funcMap globalMap dataTypeMap (Test {tesName = Identifier testName, tesExpr}) = do
  (expr, fes) <-
    runStateT
      (fromExpr ((,Nothing) <$> tesExpr))
      ( FromExprState
          { fesVars = mempty,
            fesArgs = mempty,
            fesGlobals = globalMap,
            fesImports = mempty,
            fesFunctions = funcMap,
            fesGenerated = mempty,
            fesDataTypes = dataTypeMap
          }
      )

  pure $
    WasmTest
      { wtName = testName,
        wtExpr = expr,
        wtLocals =
          snd <$> fesVars fes
      }

fromFunction ::
  (Eq ann, Show ann) =>
  M.Map FunctionName (S.Set (Ability any)) ->
  M.Map FunctionName FromExprFunc ->
  M.Map FunctionName FromExprImport ->
  M.Map Identifier FromExprGlobal ->
  M.Map DataName (Data ()) ->
  [WasmFunction] ->
  Function (Type ann) ->
  Either FromWasmError ([WasmFunction], WasmFunction)
fromFunction functionAbilities funcMap importMap globalMap dataTypeMap generatedFns fn@Function {fnPublic, fnBody, fnArgs, fnFunctionName, fnGenerics} = do
  args <-
    traverse
      ( \(FunctionArg {faName = ArgumentName ident, faType}) -> do
          wasmType <- scalarFromType faType
          pure (Identifier ident, wasmType)
      )
      fnArgs

  -- for each generic, we add a function arg
  let genericsArgs =
        ( \generic ->
            ( genericArgName generic,
              Pointer
            )
        )
          <$> fnGenerics

  let allArgs = args <> genericsArgs

  (expr, fes) <-
    runStateT
      (fromExpr (fst $ getFunctionUses fn))
      ( FromExprState
          { fesVars = mempty,
            fesArgs = allArgs,
            fesGlobals = globalMap,
            fesImports = importMap,
            fesFunctions = funcMap,
            fesGenerated = generatedFns,
            fesDataTypes = dataTypeMap
          }
      )

  retType <- scalarFromType (getOuterAnnotation fnBody)

  abilities <-
    S.map void
      <$> getAbilitiesForFunction functionAbilities fnFunctionName

  pure
    ( fesGenerated fes,
      WasmFunction
        { wfName = fnFunctionName,
          wfExpr = expr,
          wfPublic = fnPublic,
          wfArgs = snd <$> allArgs,
          wfReturnType = retType,
          wfLocals = snd <$> fesVars fes,
          wfAbilities = abilities
        }
    )

fromMemory :: Maybe (Memory (Type ann)) -> WasmMemory
fromMemory Nothing =
  WasmMemory 0 WasmLocal
fromMemory (Just (LocalMemory {lmLimit})) =
  WasmMemory lmLimit WasmLocal
fromMemory (Just (ExportedMemory {emLimit,emExportName = Identifier emExportName})) =
  WasmMemory emLimit (WasmExported emExportName)
fromMemory
  ( Just
      ( ImportedMemory
          { imExternalModule = Identifier imExternalModule,
            imExternalMemoryName = Identifier imExternalMemoryName,
            imLimit
          }
        )
    ) =
    WasmMemory imLimit (WasmImported imExternalModule imExternalMemoryName)

fromGlobal :: (Eq ann, Show ann) => Global (Type ann) -> Either FromWasmError WasmGlobal
fromGlobal (Global {glbExpr, glbMutability}) = do
  (wgExpr, _) <-
    runStateT
      (fromExpr ((,Nothing) <$> glbExpr))
      ( FromExprState
          { fesVars = mempty,
            fesArgs = mempty,
            fesGlobals = mempty,
            fesImports = mempty,
            fesFunctions = mempty,
            fesGenerated = mempty,
            fesDataTypes = mempty
          }
      )

  let wgMutable = case glbMutability of
        Mutable -> True
        Constant -> False

  wgType <- scalarFromType (getOuterAnnotation glbExpr)
  pure $ WasmGlobal {wgExpr, wgType, wgMutable}

getDataTypeMap :: [Data ann] -> M.Map DataName (Data ())
getDataTypeMap =
  foldMap
    ( \(Data {dtName, dtVars, dtConstructors}) ->
        M.singleton dtName $ Data {dtVars, dtName, dtConstructors = (fmap . fmap) void dtConstructors}
    )

fromModule ::
  (Show ann, Ord ann) =>
  Module (Type ann) ->
  Either FromWasmError WasmModule
fromModule wholeMod@(Module {mdDataTypes, mdMemory, mdTests, mdGlobals, mdImports, mdFunctions}) = do
  let moduleAbilities = getAbilitiesForModule wholeMod
  importMap <- getImportMap mdImports
  funcMap <- getFunctionMap mdFunctions
  globalMap <- getGlobalMap mdGlobals
  let dataTypeMap = getDataTypeMap mdDataTypes

  wasmGlobals <- traverse fromGlobal mdGlobals

  (generatedWasmFunctions, wasmFunctions) <-
    foldM
      ( \(generatedFns, fns) input -> do
          (generated, newFn) <-
            fromFunction (maFunctions moduleAbilities) funcMap importMap globalMap dataTypeMap generatedFns input
          pure (generated, [newFn] <> fns)
      )
      ([], [])
      (reverse mdFunctions)

  wasmImports <- traverse fromImport mdImports

  wasmTests <- traverse (fromTest funcMap globalMap dataTypeMap) mdTests

  pure $
    WasmModule
      { wmFunctions = wasmFunctions,
        wmGeneratedFunctions = generatedWasmFunctions,
        wmImports = wasmImports,
        wmMemory = fromMemory mdMemory,
        wmGlobals = wasmGlobals,
        wmTests = wasmTests
      }
