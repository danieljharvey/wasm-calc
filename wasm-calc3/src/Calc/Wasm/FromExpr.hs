{-# LANGUAGE NamedFieldPuns #-}

module Calc.Wasm.FromExpr (createModule) where

import Calc.Types.Expr
import Calc.Types.Prim
import Calc.Wasm.Types
import Data.Maybe (catMaybes)
import Data.String
import GHC.Natural
import qualified Language.Wasm.Structure as Wasm

mapWithIndex :: ((Int, a) -> b) -> [a] -> [b]
mapWithIndex f = fmap f . zip [0 ..]

fromType :: Type ann -> Wasm.ValueType
fromType I32 = Wasm.I32

fromFunction :: Int -> Function ann -> Wasm.Function
fromFunction fnIndex (Function {fnExpr, fnArgs}) =
  Wasm.Function (fromIntegral fnIndex) (fromType <$> fnArgs) (fromExpr fnExpr)

typeFromFunction :: Function ann -> Wasm.FuncType
typeFromFunction (Function {fnArgs, fnReturnType}) =
  Wasm.FuncType (fromType <$> fnArgs) [fromType fnReturnType]

exportFromFunction :: Int -> Function ann -> Maybe Wasm.Export
exportFromFunction fnIndex (Function {fnName, fnPublic = True}) =
  Just $ Wasm.Export (fromString fnName) (Wasm.ExportFunc (fromIntegral fnIndex))
exportFromFunction _ _ = Nothing

bitsizeFromType :: Type ann -> Wasm.BitSize
bitsizeFromType I32 = Wasm.BS32

instructionFromOp :: Type ann -> Op -> Wasm.Instruction Natural
instructionFromOp ty OpAdd = Wasm.IBinOp (bitsizeFromType ty) Wasm.IAdd
instructionFromOp ty OpMultiply = Wasm.IBinOp (bitsizeFromType ty) Wasm.IMul
instructionFromOp ty OpSubtract = Wasm.IBinOp (bitsizeFromType ty) Wasm.ISub
instructionFromOp ty OpEquals = Wasm.IRelOp (bitsizeFromType ty) Wasm.IEq

fromExpr :: Expr ann -> [Wasm.Instruction Natural]
fromExpr (EPrim _ (PInt i)) =
  [Wasm.I32Const $ fromIntegral i]
fromExpr (EPrim _ (PBool True)) =
  [Wasm.I32Const 1]
fromExpr (EPrim _ (PBool False)) =
  [Wasm.I32Const 0]
fromExpr (EInfix _ op a b) =
  fromExpr a <> fromExpr b <> [instructionFromOp I32 op]
fromExpr (EIf _ predExpr thenExpr elseExpr) =
  fromExpr thenExpr <> fromExpr elseExpr <> fromExpr predExpr <> [Wasm.Select]

createModule :: Module ann -> Wasm.Module
createModule (Module {modFunctions}) =
  let functions = mapWithIndex (uncurry fromFunction) modFunctions
      types = typeFromFunction <$> modFunctions
      exports = catMaybes $ mapWithIndex (uncurry exportFromFunction) modFunctions
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
