{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.ToWasm.Module (moduleToWasm) where

import Calc.Types.FunctionName
import Calc.Wasm.Allocator
import Calc.Wasm.ToWasm.Expr
import Calc.Wasm.ToWasm.Types
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import qualified Data.Text.Lazy as TL
import GHC.Natural
import qualified Language.Wasm.Structure as Wasm

mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f = fmap f . zip [0 ..]

-- | turn types into wasm types
-- void won't have a type, hence the Maybe
fromType :: WasmType -> Maybe Wasm.ValueType
fromType I8 = Just Wasm.I32
fromType I16 = Just Wasm.I32
fromType I32 = Just Wasm.I32
fromType I64 = Just Wasm.I64
fromType F32 = Just Wasm.F32
fromType F64 = Just Wasm.F64
fromType Pointer = Just Wasm.I32
fromType Void = Nothing

fromFunction :: Int -> WasmFunction -> Wasm.Function
fromFunction wfIndex (WasmFunction {wfExpr, wfLocals}) =
  let locals = fromType <$> wfLocals
   in Wasm.Function
        (fromIntegral $ wfIndex + 1)
        (catMaybes locals) -- we're dropping `Void` rather than erroring, perhaps this is bad
        (exprToWasm wfExpr)

fromImport :: Int -> WasmImport -> Wasm.Import
fromImport wfIndex (WasmImport {wiExternalModule, wiExternalFunction}) =
  Wasm.Import
    (TL.fromStrict wiExternalModule)
    (TL.fromStrict wiExternalFunction)
    (Wasm.ImportFunc $ fromIntegral wfIndex)

typeFromFunction :: WasmFunction -> Wasm.FuncType
typeFromFunction (WasmFunction {wfArgs, wfReturnType}) =
  Wasm.FuncType
    (mapMaybe fromType wfArgs)
    (maybeToList $ fromType wfReturnType)

typeFromImport :: WasmImport -> Wasm.FuncType
typeFromImport (WasmImport {wiArgs, wiReturnType}) =
  Wasm.FuncType (mapMaybe fromType wiArgs) (maybeToList $ fromType wiReturnType)

-- for now, export everything
exportFromFunction :: Int -> WasmFunction -> Maybe Wasm.Export
exportFromFunction wfIndex (WasmFunction {wfName = FunctionName wfName, wfPublic = True}) =
  Just $ Wasm.Export (TL.fromStrict wfName) (Wasm.ExportFunc (fromIntegral wfIndex + 1))
exportFromFunction _ _ = Nothing

allocatorFunction :: Natural -> Wasm.Module -> Wasm.Function
allocatorFunction offset mod' =
  let (Wasm.Function _ a b) = head (Wasm.functions mod')
   in Wasm.Function offset a b

-- add the global allocator position as a global
-- we start at 32 + any manual memory space that has been set aside
-- for messing around
globals :: WasmMemory -> [WasmGlobal] -> [Wasm.Global]
globals (WasmMemory nat _) globs =
  [ Wasm.Global
      (Wasm.Mut Wasm.I32)
      [Wasm.I32Const (fromIntegral $ nat + 32)]
  ]
    <> mapMaybe
      ( \WasmGlobal {wgExpr, wgType, wgMutable} ->
          case (wgMutable, fromType wgType) of
            (False, Just ty) ->
              Just $ Wasm.Global (Wasm.Const ty) (exprToWasm wgExpr)
            (True, Just ty) ->
              Just $ Wasm.Global (Wasm.Mut ty) (exprToWasm wgExpr)
            (_, Nothing) -> Nothing
      )
      globs

-- | if no memory has been imported, we create our own `memory`
-- instance for this module
memory :: WasmMemory -> [Wasm.Memory]
memory (WasmMemory _limit Nothing) =
  [Wasm.Memory (Wasm.Limit 1 Nothing)]
memory _ = mempty

functionImportsToWasm :: [WasmImport] -> [Wasm.Import]
functionImportsToWasm =
  mapWithIndex (uncurry fromImport)

memoryImportsToWasm :: WasmMemory -> [Wasm.Import]
memoryImportsToWasm wasmMemory =
  case wasmMemory of
    (WasmMemory _ (Just (memModule, memName))) ->
      [ Wasm.Import
          (TL.fromStrict memModule)
          (TL.fromStrict memName)
          (Wasm.ImportMemory (Wasm.Limit 1 Nothing))
      ]
    (WasmMemory _ Nothing) -> mempty

-- | we load the bump allocator module and build on top of it
moduleToWasm :: WasmModule -> Wasm.Module
moduleToWasm (WasmModule {wmMemory, wmGlobals, wmImports, wmFunctions}) =
  let functionImports = functionImportsToWasm wmImports
      offset = length functionImports
      functions = uncurry fromFunction <$> zip [offset ..] wmFunctions
      importTypes = typeFromImport <$> wmImports
      functionTypes = typeFromFunction <$> wmFunctions
      exports = mapMaybe (uncurry exportFromFunction) (zip [offset ..] wmFunctions)
      imports = memoryImportsToWasm wmMemory <> functionImports
   in moduleWithAllocator
        { Wasm.types = importTypes <> (head (Wasm.types moduleWithAllocator) : functionTypes),
          Wasm.functions = allocatorFunction (fromIntegral offset) moduleWithAllocator : functions,
          Wasm.globals = globals wmMemory wmGlobals,
          Wasm.mems = memory wmMemory,
          Wasm.tables = mempty,
          Wasm.elems = mempty,
          Wasm.datas = mempty,
          Wasm.start = Nothing,
          Wasm.imports = imports,
          Wasm.exports = exports
        }
