{-# LANGUAGE NamedFieldPuns #-}

module Calc.Wasm.ToWasm (moduleToWasm) where

import Calc.Types.Expr
import Calc.Types.FunctionName
import Calc.Types.Prim
import Calc.Wasm.Allocator
import Calc.Wasm.Types
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as TL
import GHC.Natural
import qualified Language.Wasm.Structure as Wasm

mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f = fmap f . zip [0 ..]

fromType :: WasmType -> Wasm.ValueType
fromType I32 = Wasm.I32
fromType I64 = Wasm.I64
fromType F64 = Wasm.F64
fromType Pointer = Wasm.I32

fromFunction :: Int -> WasmFunction -> Wasm.Function
fromFunction wfIndex (WasmFunction {wfExpr, wfArgs, wfLocals}) =
  let args = fromType <$> wfArgs
      locals = fromType <$> wfLocals
   in Wasm.Function
        (fromIntegral $ wfIndex + 1)
        (locals <> args)
        (fromExpr wfExpr)

typeFromFunction :: WasmFunction -> Wasm.FuncType
typeFromFunction (WasmFunction {wfArgs, wfReturnType}) =
  Wasm.FuncType (fromType <$> wfArgs) [fromType wfReturnType]

exportFromFunction :: Int -> WasmFunction -> Maybe Wasm.Export
exportFromFunction wfIndex (WasmFunction {wfName = FunctionName fnName, wfPublic = True}) =
  Just $ Wasm.Export (TL.fromStrict fnName) (Wasm.ExportFunc (fromIntegral wfIndex + 1))
exportFromFunction _ _ = Nothing

bitsizeFromType :: WasmType -> Wasm.BitSize
bitsizeFromType I32 = Wasm.BS32
bitsizeFromType I64 = Wasm.BS64
bitsizeFromType F64 = Wasm.BS64
bitsizeFromType Pointer = Wasm.BS32

instructionFromOp :: WasmType -> Op -> Wasm.Instruction Natural
instructionFromOp F64 OpAdd = Wasm.FBinOp (bitsizeFromType F64) Wasm.FAdd
instructionFromOp F64 OpMultiply = Wasm.FBinOp (bitsizeFromType F64) Wasm.FMul
instructionFromOp F64 OpSubtract = Wasm.FBinOp (bitsizeFromType F64) Wasm.FSub
instructionFromOp F64 OpEquals = Wasm.FRelOp (bitsizeFromType F64) Wasm.FEq
instructionFromOp ty OpAdd = Wasm.IBinOp (bitsizeFromType ty) Wasm.IAdd
instructionFromOp ty OpMultiply = Wasm.IBinOp (bitsizeFromType ty) Wasm.IMul
instructionFromOp ty OpSubtract = Wasm.IBinOp (bitsizeFromType ty) Wasm.ISub
instructionFromOp ty OpEquals = Wasm.IRelOp (bitsizeFromType ty) Wasm.IEq

fromExpr :: WasmExpr -> [Wasm.Instruction Natural]
fromExpr (WPrim (PInt i)) =
  [Wasm.I64Const $ fromIntegral i]
fromExpr (WPrim (PFloat f)) =
  [Wasm.F64Const $ realToFrac f]
fromExpr (WPrim (PBool True)) =
  [Wasm.I32Const 1]
fromExpr (WPrim (PBool False)) =
  [Wasm.I32Const 0]
fromExpr (WInfix ty op a b) =
  fromExpr a <> fromExpr b <> [instructionFromOp ty op]
fromExpr (WIf predExpr thenExpr elseExpr) =
  fromExpr thenExpr <> fromExpr elseExpr <> fromExpr predExpr <> [Wasm.Select]
fromExpr (WVar i) = [Wasm.GetLocal i]
fromExpr (WApply fnIndex args) =
  foldMap fromExpr args <> [Wasm.Call $ fnIndex + 1]
-- we need to store the return value so we can refer to it in multiple places
fromExpr (WAllocate i) =
  [Wasm.I32Const (fromIntegral i), Wasm.Call 0]
fromExpr (WSet index container items) =
  let fromItem (offset, ty, value) =
        let storeInstruction = case ty of
              F64 -> Wasm.F64Store (Wasm.MemArg offset 0)
              I64 -> Wasm.I64Store (Wasm.MemArg offset 0)
              I32 -> Wasm.I32Store (Wasm.MemArg offset 0)
              Pointer -> Wasm.I32Store (Wasm.MemArg offset 0)
         in [Wasm.GetLocal index] <> fromExpr value <> [storeInstruction]
   in fromExpr container
        <> [Wasm.SetLocal index]
        <> foldMap fromItem items
        <> [Wasm.GetLocal index]
fromExpr (WTupleAccess ty tup offset) =
  let loadInstruction = case ty of
        F64 -> Wasm.F64Load (Wasm.MemArg offset 0)
        I64 -> Wasm.I64Load (Wasm.MemArg offset 0)
        I32 -> Wasm.I32Load (Wasm.MemArg offset 0)
        Pointer -> Wasm.I32Load (Wasm.MemArg offset 0)
   in fromExpr tup <> [loadInstruction]

-- | we load the bump allocator module and build on top of it
moduleToWasm :: WasmModule -> Wasm.Module
moduleToWasm (WasmModule {wmFunctions}) =
  let functions = mapWithIndex (uncurry fromFunction) wmFunctions
      types = typeFromFunction <$> wmFunctions
      exports = catMaybes $ mapWithIndex (uncurry exportFromFunction) wmFunctions
   in moduleWithAllocator
        { Wasm.types = head (Wasm.types moduleWithAllocator) : types,
          Wasm.functions = head (Wasm.functions moduleWithAllocator) : functions,
          Wasm.tables = mempty,
          Wasm.elems = mempty,
          Wasm.datas = mempty,
          Wasm.start = Nothing,
          Wasm.imports = mempty,
          Wasm.exports = exports
        }
