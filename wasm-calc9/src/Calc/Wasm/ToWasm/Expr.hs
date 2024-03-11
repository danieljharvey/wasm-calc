{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.ToWasm.Expr (toWasm) where

import Calc.Types.Op
import Calc.Wasm.ToWasm.Helpers
import Calc.Wasm.ToWasm.Types
import Control.Monad.Reader
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
instructionFromOp ty OpAnd =
  Wasm.IBinOp (bitsizeFromType ty) Wasm.IAnd
instructionFromOp ty OpOr =
  Wasm.IBinOp (bitsizeFromType ty) Wasm.IOr

toWasm :: ToWasmEnv -> WasmExpr -> [Wasm.Instruction Natural]
toWasm env wasmExpr =
  runReader (exprToWasm wasmExpr) env

exprToWasm :: (MonadReader ToWasmEnv m) => WasmExpr -> m [Wasm.Instruction Natural]
exprToWasm (WPrim (WPInt32 i)) =
  pure [Wasm.I32Const i]
exprToWasm (WPrim (WPInt64 i)) =
  pure [Wasm.I64Const i]
exprToWasm (WPrim (WPFloat32 f)) =
  pure [Wasm.F32Const f]
exprToWasm (WPrim (WPFloat64 f)) =
  pure [Wasm.F64Const f]
exprToWasm (WPrim (WPBool True)) =
  pure [Wasm.I32Const 1]
exprToWasm (WPrim (WPBool False)) =
  pure [Wasm.I32Const 0]
exprToWasm (WLet index expr body) = do
  wasmExpr <- exprToWasm expr
  wasmBody <- exprToWasm body
  pure $ wasmExpr <> [Wasm.SetLocal index] <> wasmBody
exprToWasm (WSequence Void first second) = do
  exprFirst <- exprToWasm first
  exprSecond <- exprToWasm second
  pure $ exprFirst <> exprSecond
exprToWasm (WSequence _ first second) = do
  wasmFirst <- exprToWasm first
  wasmSecond <- exprToWasm second
  pure $ wasmFirst <> [Wasm.Drop] <> wasmSecond
exprToWasm (WInfix ty op a b) = do
  wasmA <- exprToWasm a
  wasmB <- exprToWasm b
  pure $ wasmA <> wasmB <> [instructionFromOp ty op]
exprToWasm (WIf tyReturn predExpr thenExpr elseExpr) = do
  wasmExpr <- exprToWasm predExpr
  wasmThen <- exprToWasm thenExpr
  wasmElse <- exprToWasm elseExpr
  pure $
    wasmExpr
      <> [ Wasm.If
             (Wasm.Inline (fromType tyReturn))
             wasmThen
             wasmElse
         ]
exprToWasm (WVar i) =
  pure [Wasm.GetLocal i]
exprToWasm (WGlobal i) = do
  offset <- globalOffset
  pure [Wasm.GetGlobal (i + offset)] -- add one as malloc function uses first global
exprToWasm (WApply fnIndex args) = do
  functionIndex <- case fnIndex of
    WasmFunctionRef i -> do
      offset <- asks functionOffset
      pure $ offset + i
    WasmImportRef i -> pure i
  wasmArgs <- traverse exprToWasm args
  pure $ mconcat wasmArgs <> [Wasm.Call functionIndex]
exprToWasm (WDrop i) = do
  fnIndex <- dropIndex
  wasmExpr <- exprToWasm i
  pure $ wasmExpr <> [Wasm.Call fnIndex]
exprToWasm (WAllocate i) = do
  fnIndex <- allocIndex
  pure
    [ Wasm.I32Const (fromIntegral i),
      Wasm.Call fnIndex
    ]
-- we need to store the return value so we can refer to it in multiple places
exprToWasm (WSet index container items) = do
  let fromItem (offset, ty, value) = do
        wasmValue <- exprToWasm value
        pure $ [Wasm.GetLocal index] <> wasmValue <> [storeInstruction ty offset]
  wasmContainer <- exprToWasm container
  wasmItems <- traverse fromItem items
  pure $
    wasmContainer
      <> [Wasm.SetLocal index]
      <> mconcat wasmItems
      <> [Wasm.GetLocal index]
exprToWasm (WTupleAccess ty tup offset) = do
  wasmExpr <- exprToWasm tup
  pure $ wasmExpr <> [loadInstruction ty offset]
exprToWasm (WLoad ty index) = do
  wasmExpr <- exprToWasm index
  pure $ wasmExpr <> [loadInstruction ty 0]
exprToWasm (WStore ty index expr) = do
  exprIndex <- exprToWasm index
  exprWasm <- exprToWasm expr
  pure $ exprIndex <> exprWasm <> [storeInstruction ty 0]
exprToWasm (WGlobalSet index expr) = do
  wasmExpr <- exprToWasm expr
  offset <- globalOffset
  pure $ wasmExpr <> [Wasm.SetGlobal (index + offset)]

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
