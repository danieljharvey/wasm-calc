{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}
module Calc.Wasm.FromExpr.Module (fromModule) where

import           Calc.Ability.Check
import           Calc.ExprUtils
import           Calc.Linearity             (getFunctionUses)
import           Calc.Types
import           Calc.Wasm.FromExpr.Expr
import           Calc.Wasm.FromExpr.Helpers
import           Calc.Wasm.FromExpr.Types
import           Calc.Wasm.ToWasm.Types
import           Control.Monad              (void)
import           Control.Monad.State
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S

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
  (Show ann) =>
  M.Map FunctionName FromExprFunc ->
  M.Map Identifier FromExprGlobal ->
  Test (Type ann) ->
  Either FromWasmError WasmTest
fromTest funcMap globalMap (Test {tesName = Identifier testName, tesExpr}) = do
  (expr, fes) <-
    runStateT
      (fromExpr ((,mempty) <$>tesExpr))
      ( FromExprState
          { fesVars = mempty,
            fesArgs = mempty,
            fesGlobals = globalMap,
            fesImports = mempty,
            fesFunctions = funcMap
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
  (Show ann) =>
  M.Map FunctionName (S.Set (Ability any)) ->
  M.Map FunctionName FromExprFunc ->
  M.Map FunctionName FromExprImport ->
  M.Map Identifier FromExprGlobal ->
  Function (Type ann) ->
  Either FromWasmError WasmFunction
fromFunction functionAbilities funcMap importMap globalMap (fn@Function {fnPublic, fnBody, fnArgs, fnFunctionName}) = do
  args <-
    traverse
      ( \(FunctionArg {faName = ArgumentName ident, faType}) -> do
          wasmType <- scalarFromType faType
          pure (Identifier ident, wasmType)
      )
      fnArgs

  (expr, fes) <-
    runStateT
      (fromExpr (fst $ getFunctionUses fn))
      ( FromExprState
          { fesVars = mempty,
            fesArgs = args,
            fesGlobals = globalMap,
            fesImports = importMap,
            fesFunctions = funcMap
          }
      )

  retType <- scalarFromType (getOuterAnnotation fnBody)

  abilities <- S.map void <$> getAbilitiesForFunction functionAbilities fnFunctionName

  pure $
    WasmFunction
      { wfName = fnFunctionName,
        wfExpr = expr,
        wfPublic = fnPublic,
        wfArgs = snd <$> args,
        wfReturnType = retType,
        wfLocals = snd <$> fesVars fes,
        wfAbilities = abilities
      }

fromMemory :: Maybe (Memory (Type ann)) -> WasmMemory
fromMemory Nothing = WasmMemory 0 Nothing
fromMemory (Just (LocalMemory {lmLimit})) =
  WasmMemory lmLimit Nothing
fromMemory
  ( Just
      ( ImportedMemory
          { imExternalModule = Identifier imExternalModule,
            imExternalMemoryName = Identifier imExternalMemoryName,
            imLimit
          }
        )
    ) =
    WasmMemory imLimit (Just (imExternalModule, imExternalMemoryName))

fromGlobal :: (Show ann) => Global (Type ann) -> Either FromWasmError WasmGlobal
fromGlobal (Global {glbExpr, glbMutability}) = do
  (wgExpr, _) <-
    runStateT
      (fromExpr ((, mempty) <$> glbExpr))
      ( FromExprState
          { fesVars = mempty,
            fesArgs = mempty,
            fesGlobals = mempty,
            fesImports = mempty,
            fesFunctions = mempty
          }
      )
  let wgMutable = case glbMutability of
        Mutable  -> True
        Constant -> False
  wgType <- scalarFromType (getOuterAnnotation glbExpr)
  pure $ WasmGlobal {wgExpr, wgType, wgMutable}

fromModule ::
  (Show ann, Ord ann) =>
  Module (Type ann) ->
  Either FromWasmError WasmModule
fromModule wholeMod@(Module {mdMemory, mdTests, mdGlobals, mdImports, mdFunctions}) = do
  let moduleAbilities = getAbilitiesForModule wholeMod
  importMap <- getImportMap mdImports
  funcMap <- getFunctionMap (fromIntegral (length importMap)) mdFunctions
  globalMap <- getGlobalMap mdGlobals

  wasmGlobals <- traverse fromGlobal mdGlobals

  wasmFunctions <-
    traverse
      (fromFunction (maFunctions moduleAbilities) funcMap importMap globalMap)
      mdFunctions

  wasmImports <- traverse fromImport mdImports

  wasmTests <- traverse (fromTest funcMap globalMap) mdTests

  pure $
    WasmModule
      { wmFunctions = wasmFunctions,
        wmImports = wasmImports,
        wmMemory = fromMemory mdMemory,
        wmGlobals = wasmGlobals,
        wmTests = wasmTests
      }
