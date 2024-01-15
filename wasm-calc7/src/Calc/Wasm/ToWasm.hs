{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.ToWasm (moduleToWasm) where

import           Calc.Types.Expr
import           Calc.Types.FunctionName
import           Calc.Types.Prim
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
fromType I32     = Just Wasm.I32
fromType I64     = Just Wasm.I64
fromType F64     = Just Wasm.F64
fromType Pointer = Just Wasm.I32
fromType Void    = Nothing

fromFunction :: Int -> WasmFunction -> Wasm.Function
fromFunction wfIndex (WasmFunction { wfExpr, wfArgs, wfLocals}) =
  let args = fromType <$> wfArgs
      locals = fromType <$> wfLocals
   in Wasm.Function
        (fromIntegral $ wfIndex + 1)
        (catMaybes $ locals <> args) -- we're dropping `Void` rather than erroring, perhaps this is bad
        (toWasm wfExpr)

fromImport :: Int -> WasmImport -> Wasm.Import
fromImport wfIndex (WasmImport {wiExternalModule, wiExternalFunction}) =
  Wasm.Import
    (TL.fromStrict wiExternalModule)
    (TL.fromStrict wiExternalFunction)
    ( Wasm.ImportFunc $ fromIntegral wfIndex )

typeFromFunction :: WasmFunction -> Wasm.FuncType
typeFromFunction (WasmFunction {wfPublic, wfArgs, wfReturnType}) =
  Wasm.FuncType (mapMaybe fromType wfArgs)
      (if wfPublic then [] else maybeToList $ fromType wfReturnType)

typeFromImport :: WasmImport -> Wasm.FuncType
typeFromImport (WasmImport {wiArgs, wiReturnType}) =
  Wasm.FuncType (mapMaybe fromType wiArgs) (maybeToList $ fromType wiReturnType)

-- for now, export everything
exportFromFunction :: Int -> WasmFunction -> Maybe Wasm.Export
exportFromFunction wfIndex (WasmFunction {wfName = FunctionName fnName})
  | fnName == "test" =
      Just $ Wasm.Export (TL.fromStrict fnName) (Wasm.ExportFunc (fromIntegral wfIndex + 1))
exportFromFunction _ _ = Nothing

bitsizeFromType :: WasmType -> Wasm.BitSize
bitsizeFromType Void    = error "bitsizeFromType Void"
bitsizeFromType I32     = Wasm.BS32
bitsizeFromType I64     = Wasm.BS64
bitsizeFromType F64     = Wasm.BS64
bitsizeFromType Pointer = Wasm.BS32

instructionFromOp :: WasmType -> Op -> Wasm.Instruction Natural
instructionFromOp F64 OpAdd      = Wasm.FBinOp (bitsizeFromType F64) Wasm.FAdd
instructionFromOp F64 OpMultiply = Wasm.FBinOp (bitsizeFromType F64) Wasm.FMul
instructionFromOp F64 OpSubtract = Wasm.FBinOp (bitsizeFromType F64) Wasm.FSub
instructionFromOp F64 OpEquals   = Wasm.FRelOp (bitsizeFromType F64) Wasm.FEq
instructionFromOp ty OpAdd       = Wasm.IBinOp (bitsizeFromType ty) Wasm.IAdd
instructionFromOp ty OpMultiply  = Wasm.IBinOp (bitsizeFromType ty) Wasm.IMul
instructionFromOp ty OpSubtract  = Wasm.IBinOp (bitsizeFromType ty) Wasm.ISub
instructionFromOp ty OpEquals    = Wasm.IRelOp (bitsizeFromType ty) Wasm.IEq

toWasm :: WasmExpr -> [Wasm.Instruction Natural]
toWasm (WPrim (PInt i)) =
  [Wasm.I64Const $ fromIntegral i]
toWasm (WPrim (PFloat f)) =
  [Wasm.F64Const $ realToFrac f]
toWasm (WPrim (PBool True)) =
  [Wasm.I32Const 1]
toWasm (WPrim (PBool False)) =
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
  foldMap toWasm args <> [Wasm.Call fnIndex ]
toWasm (WAllocate i) =
  [Wasm.I32Const (fromIntegral i), Wasm.Call 0]
-- we need to store the return value so we can refer to it in multiple places
toWasm (WSet index container items) =
  let fromItem (offset, ty, value) =
        let storeInstruction = case ty of
              F64     -> Wasm.F64Store (Wasm.MemArg offset 0)
              I64     -> Wasm.I64Store (Wasm.MemArg offset 0)
              I32     -> Wasm.I32Store (Wasm.MemArg offset 0)
              Pointer -> Wasm.I32Store (Wasm.MemArg offset 0)
              Void    -> error "WSet Void"
         in [Wasm.GetLocal index] <> toWasm value <> [storeInstruction]
   in toWasm container
        <> [Wasm.SetLocal index]
        <> foldMap fromItem items
        <> [Wasm.GetLocal index]
toWasm (WTupleAccess ty tup offset) =
  let loadInstruction = case ty of
        F64     -> Wasm.F64Load (Wasm.MemArg offset 0)
        I64     -> Wasm.I64Load (Wasm.MemArg offset 0)
        I32     -> Wasm.I32Load (Wasm.MemArg offset 0)
        Pointer -> Wasm.I32Load (Wasm.MemArg offset 0)
        Void    -> error "WTupleAccess Void"
   in toWasm tup <> [loadInstruction]

allocatorFunction :: Natural -> Wasm.Module -> Wasm.Function
allocatorFunction offset mod'
  = let (Wasm.Function _ a b) = head (Wasm.functions mod')
    in Wasm.Function offset a b

-- | we load the bump allocator module and build on top of it
moduleToWasm :: WasmModule -> Wasm.Module
moduleToWasm (WasmModule {wmImports, wmFunctions}) =
  let imports = mapWithIndex (uncurry fromImport) wmImports
      offset = length imports
      functions = uncurry fromFunction <$> zip [offset ..] wmFunctions
      importTypes = typeFromImport <$> wmImports
      functionTypes = typeFromFunction <$> wmFunctions
      exports = mapMaybe (uncurry exportFromFunction) (zip [offset ..] wmFunctions)
   in
        moduleWithAllocator
          { Wasm.types = importTypes <> (head (Wasm.types moduleWithAllocator) : functionTypes),
            Wasm.functions = allocatorFunction (fromIntegral offset) moduleWithAllocator : functions,
            Wasm.tables = mempty,
            Wasm.elems = mempty,
            Wasm.datas = mempty,
            Wasm.start = Nothing,
            Wasm.imports = imports,
            Wasm.exports = exports
          }
