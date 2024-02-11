{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.ToWasm (moduleToWasm) where

import           Calc.Types.FunctionName
import           Calc.Types.Op
import           Calc.Wasm.Allocator
import           Calc.Wasm.Types
import           Data.Maybe              (catMaybes, mapMaybe, maybeToList)
import qualified Data.Text.Lazy          as TL
import           GHC.Natural
import qualified Language.Wasm.Structure as Wasm

mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f = fmap f . zip [0 ..]

-- | turn types into wasm types
-- void won't have a type, hence the Maybe
fromType :: WasmType -> Maybe Wasm.ValueType
fromType I8      = Just Wasm.I32
fromType I16     = Just Wasm.I32
fromType I32     = Just Wasm.I32
fromType I64     = Just Wasm.I64
fromType F32     = Just Wasm.F32
fromType F64     = Just Wasm.F64
fromType Pointer = Just Wasm.I32
fromType Void    = Nothing

fromFunction :: Int -> WasmFunction -> Wasm.Function
fromFunction wfIndex (WasmFunction {wfExpr, wfLocals}) =
  let locals = fromType <$> wfLocals
   in Wasm.Function
        (fromIntegral $ wfIndex + 1)
        (catMaybes locals) -- we're dropping `Void` rather than erroring, perhaps this is bad
        (toWasm wfExpr)

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

bitsizeFromType :: WasmType -> Wasm.BitSize
bitsizeFromType Void    = error "bitsizeFromType Void"
bitsizeFromType I8      = Wasm.BS32
bitsizeFromType I16     = Wasm.BS32
bitsizeFromType I32     = Wasm.BS32
bitsizeFromType I64     = Wasm.BS64
bitsizeFromType F32     = Wasm.BS32
bitsizeFromType F64     = Wasm.BS64
bitsizeFromType Pointer = Wasm.BS32

typeIsFloat :: WasmType -> Bool
typeIsFloat F32 = True
typeIsFloat F64 = True
typeIsFloat _   = False

instructionFromOp :: WasmType -> Op -> Wasm.Instruction Natural
instructionFromOp ty OpAdd =
  if typeIsFloat ty
    then Wasm.FBinOp (bitsizeFromType ty) Wasm.FAdd
    else Wasm.IBinOp (bitsizeFromType ty) Wasm.IAdd
instructionFromOp ty OpMultiply =
  if typeIsFloat ty
    then Wasm.FBinOp (bitsizeFromType ty) Wasm.FMul
    else Wasm.IBinOp (bitsizeFromType ty) Wasm.IMul
instructionFromOp ty OpSubtract =
  if typeIsFloat ty
    then Wasm.FBinOp (bitsizeFromType ty) Wasm.FSub
    else Wasm.IBinOp (bitsizeFromType ty) Wasm.ISub
instructionFromOp ty OpEquals =
  if typeIsFloat ty
    then Wasm.FRelOp (bitsizeFromType ty) Wasm.FEq
    else Wasm.IRelOp (bitsizeFromType ty) Wasm.IEq
instructionFromOp ty OpGreaterThan =
  if typeIsFloat ty
    then Wasm.FRelOp (bitsizeFromType ty) Wasm.FGt
    else Wasm.IRelOp (bitsizeFromType ty) Wasm.IGtS
instructionFromOp ty OpGreaterThanOrEqualTo =
  if typeIsFloat ty
    then Wasm.FRelOp (bitsizeFromType ty) Wasm.FGe
    else Wasm.IRelOp (bitsizeFromType ty) Wasm.IGeS
instructionFromOp ty OpLessThan =
  if typeIsFloat ty
    then Wasm.FRelOp (bitsizeFromType ty) Wasm.FLt
    else Wasm.IRelOp (bitsizeFromType ty) Wasm.ILtS
instructionFromOp ty OpLessThanOrEqualTo =
  if typeIsFloat ty
    then Wasm.FRelOp (bitsizeFromType ty) Wasm.FLe
    else Wasm.IRelOp (bitsizeFromType ty) Wasm.ILeS

toWasm :: WasmExpr -> [Wasm.Instruction Natural]
toWasm (WPrim (WPInt32 i)) =
  [Wasm.I32Const i]
toWasm (WPrim (WPInt64 i)) =
  [Wasm.I64Const i]
toWasm (WPrim (WPFloat32 f)) =
  [Wasm.F32Const f]
toWasm (WPrim (WPFloat64 f)) =
  [Wasm.F64Const f]
toWasm (WPrim (WPBool True)) =
  [Wasm.I32Const 1]
toWasm (WPrim (WPBool False)) =
  [Wasm.I32Const 0]
toWasm (WLet index expr body) = do
  toWasm expr <> [Wasm.SetLocal index] <> toWasm body
toWasm (WSequence Void first second) =
  toWasm first <> toWasm second
toWasm (WSequence _ first second) =
  toWasm first <> [Wasm.Drop] <> toWasm second
toWasm (WInfix ty op a b) =
  toWasm a <> toWasm b <> [instructionFromOp ty op]
toWasm (WIf predExpr thenExpr elseExpr) =
  toWasm thenExpr <> toWasm elseExpr <> toWasm predExpr <> [Wasm.Select]
toWasm (WVar i) = [Wasm.GetLocal i]
toWasm (WApply fnIndex args) =
  foldMap toWasm args <> [Wasm.Call fnIndex]
toWasm (WAllocate fnIndex i) =
  [Wasm.I32Const (fromIntegral i), Wasm.Call fnIndex]
-- we need to store the return value so we can refer to it in multiple places
toWasm (WSet index container items) =
  let fromItem (offset, ty, value) =
        [Wasm.GetLocal index] <> toWasm value <> [storeInstruction ty offset]
   in toWasm container
        <> [Wasm.SetLocal index]
        <> foldMap fromItem items
        <> [Wasm.GetLocal index]
toWasm (WTupleAccess ty tup offset) =
  toWasm tup <> [loadInstruction ty offset]
toWasm (WLoad ty index) =
  [Wasm.I32Const 0, loadInstruction ty index]
toWasm (WStore ty index expr) =
  [Wasm.I32Const 0]
    <> toWasm expr
    <> [storeInstruction ty index]

loadInstruction :: WasmType -> Natural -> Wasm.Instruction Natural
loadInstruction ty offset = case ty of
  F32     -> Wasm.F32Load (Wasm.MemArg offset 0)
  F64     -> Wasm.F64Load (Wasm.MemArg offset 0)
  I8      -> Wasm.I32Load8S (Wasm.MemArg offset 0)
  I16     -> Wasm.I32Load16S (Wasm.MemArg offset 0)
  I32     -> Wasm.I32Load (Wasm.MemArg offset 0)
  I64     -> Wasm.I64Load (Wasm.MemArg offset 0)
  Pointer -> Wasm.I32Load (Wasm.MemArg offset 0)
  Void    -> error "loadInstruction Void"

storeInstruction :: WasmType -> Natural -> Wasm.Instruction Natural
storeInstruction ty offset = case ty of
  F32     -> Wasm.F32Store (Wasm.MemArg offset 0)
  F64     -> Wasm.F64Store (Wasm.MemArg offset 0)
  I8      -> Wasm.I32Store8 (Wasm.MemArg offset 0)
  I16     -> Wasm.I32Store16 (Wasm.MemArg offset 0)
  I32     -> Wasm.I32Store (Wasm.MemArg offset 0)
  I64     -> Wasm.I64Store (Wasm.MemArg offset 0)
  Pointer -> Wasm.I32Store (Wasm.MemArg offset 0)
  Void    -> error "storeInstruction Void"

allocatorFunction :: Natural -> Wasm.Module -> Wasm.Function
allocatorFunction offset mod' =
  let (Wasm.Function _ a b) = head (Wasm.functions mod')
   in Wasm.Function offset a b

-- add the global allocator position as a global
-- we start at 32 + any manual memory space that has been set aside
-- for messing around
globals :: WasmMemory -> [Wasm.Global]
globals (WasmMemory nat _) =
  [ Wasm.Global
      (Wasm.Mut Wasm.I32)
      [Wasm.I32Const (fromIntegral $ nat + 32)]
  ]

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
moduleToWasm (WasmModule {wmMemory, wmImports, wmFunctions}) =
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
            Wasm.globals = globals wmMemory,
            Wasm.mems = memory wmMemory,
            Wasm.tables = mempty,
            Wasm.elems = mempty,
            Wasm.datas = mempty,
            Wasm.start = Nothing,
            Wasm.imports = imports,
            Wasm.exports = exports
          }
