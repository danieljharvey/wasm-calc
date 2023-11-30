{-# LANGUAGE NamedFieldPuns #-}

module Calc.Wasm.ToWasm (moduleToWasm) where

import Calc.Types.Expr
import Calc.Types.FunctionName
import Calc.Types.Prim
import Calc.Wasm.Types
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as TL
import GHC.Natural
import qualified Language.Wasm.Structure as Wasm

mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f = fmap f . zip [0 ..]

fromType :: WasmType -> Wasm.ValueType
fromType I32 = Wasm.I32

fromFunction :: Int -> WasmFunction -> Wasm.Function
fromFunction wfIndex (WasmFunction {wfExpr, wfArgs}) =
  Wasm.Function (fromIntegral wfIndex) (fromType <$> wfArgs) (fromExpr wfExpr)

typeFromFunction :: WasmFunction -> Wasm.FuncType
typeFromFunction (WasmFunction {wfArgs, wfReturnType}) =
  Wasm.FuncType (fromType <$> wfArgs) [fromType wfReturnType]

exportFromFunction :: Int -> WasmFunction -> Maybe Wasm.Export
exportFromFunction wfIndex (WasmFunction {wfName = FunctionName fnName, wfPublic = True}) =
  Just $ Wasm.Export (TL.fromStrict fnName) (Wasm.ExportFunc (fromIntegral wfIndex))
exportFromFunction _ _ = Nothing

bitsizeFromType :: WasmType -> Wasm.BitSize
bitsizeFromType I32 = Wasm.BS32

instructionFromOp :: WasmType -> Op -> Wasm.Instruction Natural
instructionFromOp ty OpAdd = Wasm.IBinOp (bitsizeFromType ty) Wasm.IAdd
instructionFromOp ty OpMultiply = Wasm.IBinOp (bitsizeFromType ty) Wasm.IMul
instructionFromOp ty OpSubtract = Wasm.IBinOp (bitsizeFromType ty) Wasm.ISub
instructionFromOp ty OpEquals = Wasm.IRelOp (bitsizeFromType ty) Wasm.IEq

fromExpr :: WasmExpr -> [Wasm.Instruction Natural]
fromExpr (WPrim (PInt i)) =
  [Wasm.I32Const $ fromIntegral i]
fromExpr (WPrim (PBool True)) =
  [Wasm.I32Const 1]
fromExpr (WPrim (PBool False)) =
  [Wasm.I32Const 0]
fromExpr (WInfix op a b) =
  fromExpr a <> fromExpr b <> [instructionFromOp I32 op]
fromExpr (WIf predExpr thenExpr elseExpr) =
  fromExpr thenExpr <> fromExpr elseExpr <> fromExpr predExpr <> [Wasm.Select]
fromExpr (WVar i) = [Wasm.GetLocal i]
fromExpr (WApply fnIndex args) =
  foldMap fromExpr args <> [Wasm.Call fnIndex]

moduleToWasm :: WasmModule -> Wasm.Module
moduleToWasm (WasmModule {wmFunctions}) =
  let functions = mapWithIndex (uncurry fromFunction) wmFunctions
      types = typeFromFunction <$> wmFunctions
      exports = catMaybes $ mapWithIndex (uncurry exportFromFunction) wmFunctions
   in Wasm.Module
        { Wasm.types = types,
          Wasm.functions = functions,
          Wasm.tables = mempty,
          Wasm.mems = mempty,
          Wasm.globals = mempty,
          Wasm.elems = mempty,
          Wasm.datas = mempty,
          Wasm.start = Nothing,
          Wasm.imports = mempty,
          Wasm.exports = exports
        }
