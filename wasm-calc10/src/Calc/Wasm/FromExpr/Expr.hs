{-# LANGUAGE FlexibleContexts #-}

module Calc.Wasm.FromExpr.Expr (fromExpr) where

import Calc.ExprUtils
import Calc.Linearity (Drops (..))
import Calc.Types
import Calc.Wasm.FromExpr.Drops
  ( addDropsFromPath,
    addDropsToWasmExpr,
    dropFunctionForType,
  )
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
  ( Eq ann,
    Show ann,
    MonadError FromWasmError m,
    MonadState FromExprState m
  ) =>
  Pattern (Type ann, Maybe (Drops ann)) ->
  Expr (Type ann, Maybe (Drops ann)) ->
  Expr (Type ann, Maybe (Drops ann)) ->
  m WasmExpr
fromLet pat expr rest = do
  let paths = patternToPaths (fst <$> pat) id
  if null paths
    then do
      wasmTy <- liftEither $ scalarFromType $ fst $ getOuterPatternAnnotation pat
      WSequence wasmTy <$> fromExpr expr <*> fromExpr rest
    else do
      -- get type of the main expr
      wasmType <- liftEither $ scalarFromType $ fst $ getOuterAnnotation expr
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

      -- drop items in the match expr we will no longer need
      dropPaths <-
        traverse (addDropsFromPath index) (patternToDropPaths pat id)

      -- take care of stuff we've pattern matched into oblivion
      let wasmRestWithDrops = foldr (WSequence Void) wasmRest dropPaths

      -- `let i = <expr>; let a = i.1; let b = i.2; <rest>....`
      pure $
        WLet index wasmExpr $
          foldr
            ( \(bindingIndex, fetchExpr) thisExpr ->
                WLet bindingIndex fetchExpr thisExpr
            )
            wasmRestWithDrops
            indexes

fromMatch ::
  (MonadState FromExprState m, MonadError FromWasmError m, Eq ann, Show ann) =>
  Expr (Type ann, Maybe (Drops ann)) ->
  NE.NonEmpty (Pattern (Type ann, Maybe (Drops ann)), Expr (Type ann, Maybe (Drops ann))) ->
  m WasmExpr
fromMatch expr _pats = do
  wasmExpr <- fromExpr expr

  -- TODO: actually make this do something good
  pure wasmExpr

fromExprWithDrops ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Eq ann,
    Show ann
  ) =>
  Expr (Type ann, Maybe (Drops ann)) ->
  m WasmExpr
fromExprWithDrops expr = do
  -- convert the expr
  wasmExpr <- fromExpr expr

  -- get any drops in this arm
  let (_, drops) = getOuterAnnotation expr

  addDropsToWasmExpr drops wasmExpr

fromExpr ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann,
    Eq ann
  ) =>
  Expr (Type ann, Maybe (Drops ann)) ->
  m WasmExpr
fromExpr (EPrim (ty, _) prim) =
  WPrim <$> fromPrim ty prim
fromExpr (EMatch _ expr pats) =
  fromMatch expr pats
fromExpr (EConstructor {}) = error "fromExpr EConstructor"
fromExpr (EBlock _ expr) =
  -- ignore block. todo: will we lose drops here
  fromExpr expr
fromExpr (EAnn _ _ expr) =
  -- ignore type annotations. todo: will we lose drops here
  fromExpr expr
fromExpr (ELet _ pat expr rest) =
  fromLet pat expr rest
fromExpr (EInfix _ op a b) = do
  -- we're assuming that the types of `a` and `b` are the same
  -- we want the type of the args, not the result
  scalar <- liftEither $ scalarFromType $ fst $ getOuterAnnotation a
  WInfix scalar op <$> fromExpr a <*> fromExpr b
fromExpr (EIf (ty, _) predE thenE elseE) = do
  wasmType <- liftEither $ scalarFromType ty
  WIf wasmType
    <$> fromExpr predE
    <*> fromExprWithDrops thenE
    <*> fromExprWithDrops elseE
fromExpr (EVar _ ident) = do
  (WVar <$> lookupIdent ident)
    `catchError` \_ -> WGlobal <$> lookupGlobal ident
fromExpr (EApply _ funcName args) = do
  (fIndex, fGenerics, fArgTypes) <- lookupFunction funcName
  let types =
        monomorphiseTypes
          fGenerics
          fArgTypes
          (void . fst . getOuterAnnotation <$> args)
  dropArgs <- traverse (dropFunctionForType . snd) types
  wasmArgs <- traverse fromExpr args
  pure $ WApply fIndex (wasmArgs <> dropArgs)
fromExpr (ETuple (ty, _) a as) = do
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
            <$> liftEither (scalarFromType (fst $ getOuterAnnotation item))
            <*> fromExpr item
      )
      allItems
fromExpr (EBox (ty, _) inner) = do
  innerWasmType <- liftEither $ scalarFromType $ fst $ getOuterAnnotation inner
  containerWasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing containerWasmType
  boxed index innerWasmType <$> fromExpr inner
fromExpr (ELoad (ty, _) index) = do
  wasmType <- liftEither $ scalarFromType ty
  WLoad wasmType <$> fromExpr index
fromExpr (EStore _ index expr) = do
  wasmType <- liftEither $ scalarFromType $ fst $ getOuterAnnotation expr
  WStore wasmType <$> fromExpr index <*> fromExpr expr
fromExpr (ESet _ ident expr) = do
  index <- lookupGlobal ident
  WGlobalSet index <$> fromExpr expr
