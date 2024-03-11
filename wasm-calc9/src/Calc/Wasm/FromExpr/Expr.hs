{-# LANGUAGE FlexibleContexts #-}

module Calc.Wasm.FromExpr.Expr (fromExpr) where

import Calc.ExprUtils
import Calc.Types
import Calc.Wasm.FromExpr.Helpers
import Calc.Wasm.FromExpr.Patterns
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Helpers
import Calc.Wasm.ToWasm.Types
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

fromLet ::
  ( Show ann,
    MonadError FromWasmError m,
    MonadState FromExprState m
  ) =>
  Pattern (Type ann) ->
  Expr (Type ann) ->
  Expr (Type ann) ->
  m WasmExpr
fromLet pat expr rest = do
  let paths = patternToPaths pat id
  if null paths
    then do
      wasmTy <- liftEither $ scalarFromType (getOuterPatternAnnotation pat)
      WSequence wasmTy <$> fromExpr expr <*> fromExpr rest
    else do
      -- get type of the main expr
      wasmType <- liftEither (scalarFromType (getOuterAnnotation expr))
      -- first we make a nameless binding of the whole value
      index <- addLocal Nothing wasmType
      -- convert expr
      wasmExpr <- fromExpr expr
      -- turn patterns into indexes and expressions
      indexes <-
        traverse
          ( \(ident, path) -> do
              let ty = typeFromPath path
              -- wasm type of var
              bindingType <- liftEither (scalarFromType ty)
              -- named binding
              bindingIndex <- addLocal (Just ident) bindingType
              -- get type we're going to be grabbing
              fetchExpr <- fromPath index path
              -- return some stuff
              pure (bindingIndex, fetchExpr)
          )
          (M.toList paths)

      -- convert the rest
      wasmRest <- fromExpr rest

      -- if we're matching on an object, drop it when we're done
      let restWithDrop =
            case wasmType of
              Pointer -> wasmRest -- WSequence wasmType (WDrop 1 wasmExpr) wasmRest
              _ -> wasmRest

      -- `let i = <expr>; let a = i.1; let b = i.2; <rest>....`
      pure $
        WLet index wasmExpr $
          foldr
            ( \(bindingIndex, fetchExpr) thisExpr ->
                WLet bindingIndex fetchExpr thisExpr
            )
            restWithDrop
            indexes

-- | we use a combination of the value and the type
fromPrim :: (MonadError FromWasmError m) => Type ann -> Prim -> m WasmPrim
fromPrim _ (PBool b) = pure $ WPBool b
fromPrim (TPrim _ TFloat32) (PFloatLit f) =
  pure $ WPFloat32 (realToFrac f)
fromPrim (TPrim _ TFloat64) (PFloatLit f) =
  pure $ WPFloat64 f
fromPrim (TPrim _ TInt8) (PIntLit i) =
  pure (WPInt32 (fromIntegral i))
fromPrim (TPrim _ TInt16) (PIntLit i) =
  pure (WPInt32 (fromIntegral i))
fromPrim (TPrim _ TInt32) (PIntLit i) =
  pure (WPInt32 (fromIntegral i))
fromPrim (TPrim _ TInt64) (PIntLit i) =
  pure (WPInt64 (fromIntegral i))
fromPrim ty prim =
  throwError $ PrimWithNonNumberType prim (void ty)

fromExpr ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann
  ) =>
  Expr (Type ann) ->
  m WasmExpr
fromExpr (EPrim ty prim) =
  WPrim <$> fromPrim ty prim
fromExpr (EBlock _ expr) =
  -- ignore block
  fromExpr expr
fromExpr (EAnn _ _ expr) =
  -- ignore type annotations
  fromExpr expr
fromExpr (ELet _ pat expr rest) =
  fromLet pat expr rest
fromExpr (EInfix _ op a b) = do
  -- we're assuming that the types of `a` and `b` are the same
  -- we want the type of the args, not the result
  scalar <- liftEither $ scalarFromType (getOuterAnnotation a)
  WInfix scalar op <$> fromExpr a <*> fromExpr b
fromExpr (EIf ty predE thenE elseE) = do
  wasmType <- liftEither $ scalarFromType ty
  WIf wasmType <$> fromExpr predE <*> fromExpr thenE <*> fromExpr elseE
fromExpr (EVar _ ident) = do
  (WVar <$> lookupIdent ident)
    `catchError` \_ -> WGlobal <$> lookupGlobal ident
fromExpr (EApply _ funcName args) = do
  fIndex <- lookupFunction funcName
  WApply fIndex
    <$> traverse fromExpr args
fromExpr (ETuple ty a as) = do
  wasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing wasmType
  let allItems = zip [0 ..] (a : NE.toList as)
      tupleLength = memorySizeForType ty
      allocate = WAllocate (fromIntegral tupleLength)
      offsetList = getOffsetList ty
  WSet index allocate
    <$> traverse
      ( \(i, item) ->
          (,,) (offsetList !! i)
            <$> liftEither (scalarFromType (getOuterAnnotation item))
            <*> fromExpr item
      )
      allItems
fromExpr (EBox ty inner) = do
  innerWasmType <- liftEither $ scalarFromType (getOuterAnnotation inner)
  containerWasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing containerWasmType
  boxed index innerWasmType <$> fromExpr inner
fromExpr (ELoad ty index) = do
  wasmType <- liftEither $ scalarFromType ty
  WLoad wasmType <$> fromExpr index
fromExpr (EStore _ index expr) = do
  wasmType <- liftEither $ scalarFromType (getOuterAnnotation expr)
  WStore wasmType <$> fromExpr index <*> fromExpr expr
fromExpr (ESet _ ident expr) = do
  index <- lookupGlobal ident
  WGlobalSet index <$> fromExpr expr
