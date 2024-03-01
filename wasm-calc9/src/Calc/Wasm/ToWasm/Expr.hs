{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.ToWasm.Expr (exprToWasm) where

import Calc.Types.Op
import Calc.Wasm.ToWasm.Helpers
import Calc.Wasm.ToWasm.Types
import GHC.Natural
import qualified Language.Wasm.Structure as Wasm

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

bitsizeFromType :: WasmType -> Wasm.BitSize
bitsizeFromType Void = error "bitsizeFromType Void"
bitsizeFromType I8 = Wasm.BS32
bitsizeFromType I16 = Wasm.BS32
bitsizeFromType I32 = Wasm.BS32
bitsizeFromType I64 = Wasm.BS64
bitsizeFromType F32 = Wasm.BS32
bitsizeFromType F64 = Wasm.BS64
bitsizeFromType Pointer = Wasm.BS32

typeIsFloat :: WasmType -> Bool
typeIsFloat F32 = True
typeIsFloat F64 = True
typeIsFloat _ = False

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

exprToWasm :: WasmExpr -> [Wasm.Instruction Natural]
exprToWasm (WPrim (WPInt32 i)) =
  [Wasm.I32Const i]
exprToWasm (WPrim (WPInt64 i)) =
  [Wasm.I64Const i]
exprToWasm (WPrim (WPFloat32 f)) =
  [Wasm.F32Const f]
exprToWasm (WPrim (WPFloat64 f)) =
  [Wasm.F64Const f]
exprToWasm (WPrim (WPBool True)) =
  [Wasm.I32Const 1]
exprToWasm (WPrim (WPBool False)) =
  [Wasm.I32Const 0]
exprToWasm (WLet index expr body) = do
  exprToWasm expr <> [Wasm.SetLocal index] <> exprToWasm body
exprToWasm (WSequence Void first second) =
  exprToWasm first <> exprToWasm second
exprToWasm (WSequence _ first second) =
  exprToWasm first <> [Wasm.Drop] <> exprToWasm second
exprToWasm (WInfix ty op a b) =
  exprToWasm a <> exprToWasm b <> [instructionFromOp ty op]
exprToWasm (WIf tyReturn predExpr thenExpr elseExpr) =
  exprToWasm predExpr
    <> [ Wasm.If
           (Wasm.Inline (fromType tyReturn))
           (exprToWasm thenExpr)
           (exprToWasm elseExpr)
       ]
exprToWasm (WVar i) = [Wasm.GetLocal i]
exprToWasm (WGlobal i) = [Wasm.GetGlobal (i + globalOffset)] -- add one as malloc function uses first global
exprToWasm (WApply fnIndex args) =
  foldMap exprToWasm args <> [Wasm.Call fnIndex]
exprToWasm (WAllocate fnIndex i) =
  [Wasm.I32Const (fromIntegral i), Wasm.Call fnIndex]
-- we need to store the return value so we can refer to it in multiple places
exprToWasm (WSet index container items) =
  let fromItem (offset, ty, value) =
        [Wasm.GetLocal index] <> exprToWasm value <> [storeInstruction ty offset]
   in exprToWasm container
        <> [Wasm.SetLocal index]
        <> foldMap fromItem items
        <> [Wasm.GetLocal index]
exprToWasm (WTupleAccess ty tup offset) =
  exprToWasm tup <> [loadInstruction ty offset]
exprToWasm (WLoad ty index) =
  exprToWasm index <> [loadInstruction ty 0]
exprToWasm (WStore ty index expr) =
  exprToWasm index
    <> exprToWasm expr
    <> [storeInstruction ty 0]
exprToWasm (WGlobalSet index expr) =
  exprToWasm expr <> [Wasm.SetGlobal (index + globalOffset)]

loadInstruction :: WasmType -> Natural -> Wasm.Instruction Natural
loadInstruction ty offset = case ty of
  F32 -> Wasm.F32Load (Wasm.MemArg offset 0)
  F64 -> Wasm.F64Load (Wasm.MemArg offset 0)
  I8 -> Wasm.I32Load8S (Wasm.MemArg offset 0)
  I16 -> Wasm.I32Load16S (Wasm.MemArg offset 0)
  I32 -> Wasm.I32Load (Wasm.MemArg offset 0)
  I64 -> Wasm.I64Load (Wasm.MemArg offset 0)
  Pointer -> Wasm.I32Load (Wasm.MemArg offset 0)
  Void -> error "loadInstruction Void"

storeInstruction :: WasmType -> Natural -> Wasm.Instruction Natural
storeInstruction ty offset = case ty of
  F32 -> Wasm.F32Store (Wasm.MemArg offset 0)
  F64 -> Wasm.F64Store (Wasm.MemArg offset 0)
  I8 -> Wasm.I32Store8 (Wasm.MemArg offset 0)
  I16 -> Wasm.I32Store16 (Wasm.MemArg offset 0)
  I32 -> Wasm.I32Store (Wasm.MemArg offset 0)
  I64 -> Wasm.I64Store (Wasm.MemArg offset 0)
  Pointer -> Wasm.I32Store (Wasm.MemArg offset 0)
  Void -> error "storeInstruction Void"
