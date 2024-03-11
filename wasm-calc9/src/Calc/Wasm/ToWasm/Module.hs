{-# LANGUAGE NamedFieldPuns #-}

module Calc.Wasm.ToWasm.Module (moduleToWasm) where

import Calc.Types.FunctionName
import Calc.Wasm.Allocator
import Calc.Wasm.ToWasm.Expr
import Calc.Wasm.ToWasm.Helpers
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

fromFunction :: ToWasmEnv -> Int -> WasmFunction -> Wasm.Function
fromFunction env wfIndex (WasmFunction {wfExpr, wfLocals}) =
  let locals = fromType <$> wfLocals
   in Wasm.Function
        (fromIntegral $ wfIndex + fromIntegral (functionOffset env))
        (catMaybes locals) -- we're dropping `Void` rather than erroring, perhaps this is bad
        (toWasm env wfExpr)

fromTest :: ToWasmEnv -> Int -> WasmTest -> Wasm.Function
fromTest env wfIndex (WasmTest {wtExpr, wtLocals}) =
  let locals = fromType <$> wtLocals
   in Wasm.Function
        (fromIntegral $ wfIndex + fromIntegral (functionOffset env))
        (catMaybes locals)
        (toWasm env wtExpr)

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

typeFromTest :: WasmTest -> Wasm.FuncType
typeFromTest (WasmTest {}) =
  Wasm.FuncType
    mempty
    [Wasm.I32]

exportFromFunction :: ToWasmEnv -> Int -> WasmFunction -> Maybe Wasm.Export
exportFromFunction env wfIndex (WasmFunction {wfName = FunctionName wfName, wfPublic = True}) =
  Just $
    Wasm.Export
      (TL.fromStrict wfName)
      (Wasm.ExportFunc (fromIntegral wfIndex + fromIntegral (functionOffset env)))
exportFromFunction _ _ _ = Nothing

exportFromTest :: ToWasmEnv -> Int -> WasmTest -> Wasm.Export
exportFromTest env wfIndex wt =
  Wasm.Export
    (TL.fromStrict $ testName wt)
    (Wasm.ExportFunc (fromIntegral wfIndex + fromIntegral (functionOffset env)))

-- take all functions from the allocator module, and offset their function
-- numbers so they live after the imports
allocatorFunctions :: Natural -> Wasm.Module -> [Wasm.Function]
allocatorFunctions offset mod' =
  let addOffset (i, Wasm.Function _ a b) =
        Wasm.Function (offset + i) a b
      numberedFunctions = zip [0 ..] (Wasm.functions mod')
   in case addOffset <$> numberedFunctions of
        headF : tailF -> fixAllocatorFunction offset headF : tailF
        [] -> []

fixAllocatorFunction :: Natural -> Wasm.Function -> Wasm.Function
fixAllocatorFunction offset (Wasm.Function a b items) =
  let fixExpr item = case item of
        Wasm.Call _ ->
          Wasm.Call offset
        Wasm.Block bt body ->
          Wasm.Block bt (fixExpr <$> body)
        Wasm.Loop bt body ->
          Wasm.Loop bt (fixExpr <$> body)
        Wasm.If bt trueE falseE ->
          Wasm.If bt (fixExpr <$> trueE) (fixExpr <$> falseE)
        other -> other
   in Wasm.Function a b (fixExpr <$> items)

-- | manually creating the global needed by the allocator
allocatorGlobals :: WasmMemory -> [Wasm.Global]
allocatorGlobals (WasmMemory nat _) =
  [ Wasm.Global
      (Wasm.Mut Wasm.I32)
      [Wasm.I32Const (fromIntegral $ nat + 32)]
  ]

-- add the global allocator position as a global
-- we start at 32 + any manual memory space that has been set aside
-- for messing around
globals :: ToWasmEnv -> [WasmGlobal] -> [Wasm.Global]
globals env =
  mapMaybe
    ( \WasmGlobal {wgExpr, wgType, wgMutable} ->
        case (wgMutable, fromType wgType) of
          (False, Just ty) ->
            Just $ Wasm.Global (Wasm.Const ty) (toWasm env wgExpr)
          (True, Just ty) ->
            Just $ Wasm.Global (Wasm.Mut ty) (toWasm env wgExpr)
          (_, Nothing) -> Nothing
    )

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

-- | we can't get these out of the file directly
-- so these are
-- alloc :: I32 -> I32
-- drop: I32 -> void
allocatorTypes :: [Wasm.FuncType]
allocatorTypes =
  [ Wasm.FuncType [Wasm.I32] [Wasm.I32],
    Wasm.FuncType [Wasm.I32] []
  ]

-- | we load the bump allocator module and build on top of it
moduleToWasm :: WasmModule -> Wasm.Module
moduleToWasm (WasmModule {wmMemory, wmGlobals, wmImports, wmTests, wmFunctions}) =
  let functionImports =
        functionImportsToWasm wmImports
      importOffset =
        length functionImports
      allocGlobals =
        allocatorGlobals wmMemory
      allocFunctions =
        allocatorFunctions (fromIntegral importOffset) moduleWithAllocator
      env =
        ToWasmEnv
          { tweImportsOffset = fromIntegral importOffset,
            tweGlobalOffset = fromIntegral (length allocGlobals),
            tweFunctionsOffset = fromIntegral (length allocFunctions)
          }
      functions =
        uncurry (fromFunction env) <$> zip [0 ..] wmFunctions
      testsOffset =
        importOffset + length functions
      tests =
        uncurry (fromTest env) <$> zip [testsOffset ..] wmTests
      importTypes =
        typeFromImport <$> wmImports
      functionTypes =
        typeFromFunction <$> wmFunctions
      testTypes =
        typeFromTest <$> wmTests
      exports =
        mapMaybe (uncurry $ exportFromFunction env) (zip [0 ..] wmFunctions)
          <> fmap (uncurry $ exportFromTest env) (zip [testsOffset ..] wmTests)
      imports = memoryImportsToWasm wmMemory <> functionImports
   in moduleWithAllocator
        { Wasm.types = importTypes <> allocatorTypes <> functionTypes <> testTypes,
          Wasm.functions = allocFunctions <> functions <> tests,
          Wasm.globals = allocGlobals <> globals env wmGlobals,
          Wasm.mems = memory wmMemory,
          Wasm.tables = mempty,
          Wasm.elems = mempty,
          Wasm.datas = mempty,
          Wasm.start = Nothing,
          Wasm.imports = imports,
          Wasm.exports = exports
        }
