{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.ToWasm.Expr (toWasm) where

import           Calc.Types.Op
import           Calc.Wasm.ToWasm.Helpers
import           Calc.Wasm.ToWasm.Types
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Foldable            (traverse_)
import           GHC.Natural
import qualified Language.Wasm.Structure  as Wasm

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
instructionFromOp ty OpAnd =
  Wasm.IBinOp (bitsizeFromType ty) Wasm.IAnd
instructionFromOp ty OpOr =
  Wasm.IBinOp (bitsizeFromType ty) Wasm.IOr

toWasm :: ToWasmEnv -> WasmExpr -> [Wasm.Instruction Natural]
toWasm env wasmExpr =
  snd $ runIdentity $ runWriterT $ runReaderT (exprToWasm wasmExpr) env

-- on each "iteration", output the normal wasm
-- and then any drops
exprToWasm :: (MonadReader ToWasmEnv m,
    MonadWriter [Wasm.Instruction Natural] m) => WasmExpr -> m ()
exprToWasm wasmExpr = exprToWasmInternal wasmExpr >> dropsToWasm wasmExpr


wasmDropToWasm :: (MonadReader ToWasmEnv m, MonadWriter [Wasm.Instruction Natural] m) => WasmDrop -> m ()
wasmDropToWasm (WasmDrop nats) =
  let dropIt i = do
              fnIndex <- dropIndex
              tell [Wasm.I32Const (fromIntegral i), Wasm.Call fnIndex]
   in traverse_ dropIt nats

-- emit wasm for whichever drops we need to do
dropsToWasm :: (MonadReader ToWasmEnv m,
    MonadWriter [Wasm.Instruction Natural] m) => WasmExpr -> m ()
dropsToWasm (WPrim drops _)            = wasmDropToWasm drops
dropsToWasm (WInfix drops _ _ _ _)     = wasmDropToWasm drops
dropsToWasm (WLet drops _ _ _)         = wasmDropToWasm drops
dropsToWasm (WSequence drops _ _ _)    = wasmDropToWasm drops
dropsToWasm (WIf drops _ _ _ _)        = wasmDropToWasm drops
dropsToWasm (WVar drops _)             = wasmDropToWasm drops
dropsToWasm (WGlobal drops _)          = wasmDropToWasm drops
dropsToWasm (WApply drops _ _)         = wasmDropToWasm drops
dropsToWasm (WAllocate drops _ )       = wasmDropToWasm drops
dropsToWasm (WDrop drops _)            = wasmDropToWasm drops
dropsToWasm WAllocCount                = pure ()
dropsToWasm (WSet drops _ _ _)         = wasmDropToWasm drops
dropsToWasm (WTupleAccess drops _ _ _) = wasmDropToWasm drops
dropsToWasm (WLoad drops _ _)          = wasmDropToWasm drops
dropsToWasm (WStore drops _ _ _ )      = wasmDropToWasm drops
dropsToWasm (WGlobalSet drops _ _)     = wasmDropToWasm drops

exprToWasmInternal :: (MonadReader ToWasmEnv m,
    MonadWriter [Wasm.Instruction Natural] m) => WasmExpr -> m ()
exprToWasmInternal (WPrim _ (WPInt32 i)) =
  tell [Wasm.I32Const i]
exprToWasmInternal (WPrim _ (WPInt64 i)) =
  tell [Wasm.I64Const i]
exprToWasmInternal (WPrim _ (WPFloat32 f)) =
  tell [Wasm.F32Const f]
exprToWasmInternal (WPrim _ (WPFloat64 f)) =
  tell [Wasm.F64Const f]
exprToWasmInternal (WPrim _ (WPBool True)) =
  tell [Wasm.I32Const 1]
exprToWasmInternal (WPrim _ (WPBool False)) =
  tell [Wasm.I32Const 0]
exprToWasmInternal (WLet _ index expr body) = do
  exprToWasm expr
  tell [Wasm.SetLocal index]
  exprToWasm body
exprToWasmInternal (WSequence _ Void first second) = do
  exprToWasm first
  exprToWasm second
exprToWasmInternal (WSequence _ _ first second) = do
  exprToWasm first
  tell [Wasm.Drop]
  exprToWasm second
exprToWasmInternal (WInfix _ ty op a b) = do
  exprToWasm a
  exprToWasm b
  tell [instructionFromOp ty op]
exprToWasmInternal (WIf _ tyReturn predExpr thenExpr elseExpr) = do
  exprToWasm predExpr
  wasmThen <- execWriterT $ exprToWasm thenExpr
  wasmElse <- execWriterT $  exprToWasm elseExpr
  tell
      [ Wasm.If
             (Wasm.Inline (fromType tyReturn))
             wasmThen
             wasmElse
         ]
exprToWasmInternal (WVar _ i) =
  tell [Wasm.GetLocal i]
exprToWasmInternal (WGlobal _ i) = do
  offset <- globalOffset
  tell [Wasm.GetGlobal (i + offset)] -- add one as malloc function uses first global
exprToWasmInternal (WApply _ fnIndex args) = do
  functionIndex <- case fnIndex of
    WasmFunctionRef i -> do
      offset <- asks functionOffset
      pure $ offset + i
    WasmImportRef i -> pure i
  traverse_ exprToWasm args
  tell [Wasm.Call functionIndex]
exprToWasmInternal (WDrop _ i) = do
  fnIndex <- dropIndex
  exprToWasm i
  tell [Wasm.Call fnIndex]
exprToWasmInternal WAllocCount = do
  fnIndex <- allocCountIndex
  tell [Wasm.Call fnIndex]
exprToWasmInternal (WAllocate _ i) = do
  fnIndex <- allocIndex
  tell
    [ Wasm.I32Const (fromIntegral i),
      Wasm.Call fnIndex
    ]
-- we need to store the return value so we can refer to it in multiple places
exprToWasmInternal (WSet _ index container items) = do
  let fromItem (offset, ty, value) = do
        tell [Wasm.GetLocal index]
        exprToWasm value
        tell [storeInstruction ty offset]
  exprToWasm container
  tell [Wasm.SetLocal index]
  traverse_ fromItem items
  tell [Wasm.GetLocal index]
exprToWasmInternal (WTupleAccess _ ty tup offset) = do
  exprToWasm tup
  tell [loadInstruction ty offset]
exprToWasmInternal (WLoad _ ty index) = do
  exprToWasm index
  tell [loadInstruction ty 0]
exprToWasmInternal (WStore _ ty index expr) = do
  exprToWasm index
  exprToWasm expr
  tell [storeInstruction ty 0]
exprToWasmInternal (WGlobalSet _ index expr) = do
  exprToWasm expr
  offset <- globalOffset
  tell [Wasm.SetGlobal (index + offset)]

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
