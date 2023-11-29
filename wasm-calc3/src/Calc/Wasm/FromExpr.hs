{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
  {-# LANGUAGE OverloadedStrings #-}
module Calc.Wasm.FromExpr (fromModule) where

import Calc.Types.Function
import Calc.Types.Module
import Calc.Wasm.Types
import Calc.Types.Expr
import Calc.Types.Type

-- | take our regular module and do the book keeping to get it ready for Wasm
-- town
data FromWasmError = FunctionTypeNotScalar
  deriving stock (Eq, Ord, Show)

fromType :: Type ann -> Either FromWasmError WasmType
fromType (TPrim _ TInt) = pure I32
fromType (TPrim _ TBool) = pure I32
fromType (TFunction {}) = Left FunctionTypeNotScalar

fromExpr :: Expr ann -> WasmExpr
fromExpr (EPrim _ prim) = WPrim prim
fromExpr (EInfix _ op a b) = WInfix op (fromExpr a) (fromExpr b)
fromExpr (EIf _ predE thenE elseE)
  = WIf (fromExpr predE) (fromExpr thenE) (fromExpr elseE)
fromExpr (EVar _ _ ) = WVar 1 -- need to make this Stateful fresh blah
fromExpr (EApply _ _funcName args)
  = WApply 1 (fromExpr <$> args) -- need to look up the function name in some sort of state

fromFunction :: Function ann -> Either FromWasmError WasmFunction 
fromFunction (Function {fnBody,fnArgs,fnFunctionName}) = do
  args <- traverse (fromType . snd) fnArgs
  pure $
    WasmFunction
      { wfName = fnFunctionName, 
        wfExpr = fromExpr fnBody,
        wfPublic = False,
        wfArgs = args, 
        wfReturnType = I32 -- a pure guess, we should use the typed module here and grab the type from `ann`
      }

fromModule :: Module ann -> Either FromWasmError WasmModule 
fromModule (Module {mdExpr, mdFunctions}) = do
  let mainFunction =
        WasmFunction
          { wfName = "main",
            wfExpr = fromExpr mdExpr,
            wfPublic = True,
            wfArgs = mempty,
            wfReturnType = I32
          }
  wasmFunctions <- traverse fromFunction mdFunctions
  pure $
    WasmModule
      { wmFunctions = mainFunction : wasmFunctions
      }
