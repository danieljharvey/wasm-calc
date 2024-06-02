{-# LANGUAGE FlexibleContexts #-}

module Calc.Wasm.FromExpr.Expr (fromExpr) where

import           Calc.ExprUtils
import           Calc.Linearity              (Drops (..))
import           Calc.Types
import           Calc.Wasm.FromExpr.Drops
import           Calc.Wasm.FromExpr.Helpers
import           Calc.Wasm.FromExpr.Patterns
import           Calc.Wasm.FromExpr.Types
import           Calc.Wasm.ToWasm.Helpers
import           Calc.Wasm.ToWasm.Types
import           Control.Monad               (foldM, void)
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Strict             as M

fromLet ::
  ( Eq ann,
    Show ann,
    MonadError FromWasmError m,
    MonadState FromExprState m
  ) =>
  (Type ann, Maybe (Drops ann)) ->
  Pattern (Type ann, Maybe (Drops ann)) ->
  Expr (Type ann, Maybe (Drops ann)) ->
  Expr (Type ann, Maybe (Drops ann)) ->
  m WasmExpr
fromLet (_, drops) pat expr rest = do
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

      -- drop identifiers we will no longer need
      wasmRestWithDrops <- addDropsToWasmExpr drops wasmRest

      -- drop items in the match expr we will no longer need
      dropPaths <-
        traverse (fmap WDrop <$> fromPath index) (patternToDropPaths pat id)

      -- take care of stuff we've pattern matched into oblivion
      let wasmRestWithManyDrops = foldr (WSequence Void) wasmRestWithDrops dropPaths

      -- `let i = <expr>; let a = i.1; let b = i.2; <rest>....`
      pure $
        WLet index wasmExpr $
          foldr
            ( \(bindingIndex, fetchExpr) thisExpr ->
                WLet bindingIndex fetchExpr thisExpr
            )
            wasmRestWithManyDrops
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

addDropsToWasmExpr ::
  ( MonadState FromExprState m,
    MonadError FromWasmError m
  ) =>
  Maybe (Drops ann) ->
  WasmExpr ->
  m WasmExpr
addDropsToWasmExpr drops wasmExpr =
  -- drop identifiers we will no longer need
  case drops of
    Just (DropIdentifiers idents) -> do
      nats <- traverse (\(ident, ty) -> (,) <$> lookupIdent ident <*> pure ty) idents
      foldM
        ( \restExpr (index, ty) -> do
            dropWasm <- case ty of
              TVar _ typeVar -> do
                -- generics must have been passed in as function args
                nat <- lookupIdent (genericArgName typeVar)
                pure (WApplyIndirect (WVar nat) [WVar index])
              _ -> do
                -- generate a new fancy drop function
                dropFunc <- createDropFunction 1 ty
                dropVar <- addGeneratedFunction dropFunc
                pure (WApply dropVar [WVar index])
            pure $ WSequence Void dropWasm restExpr
        )
        wasmExpr
        nats
    Just DropMe -> pure wasmExpr -- TODO: what does this mean? anything?
    Nothing -> pure wasmExpr

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
fromExpr (EBlock _ expr) =
  -- ignore block. todo: will we lose drops here
  fromExpr expr
fromExpr (EAnn _ _ expr) =
  -- ignore type annotations. todo: will we lose drops here
  fromExpr expr
fromExpr (ELet ann pat expr rest) =
  fromLet ann pat expr rest
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
        calculateMonomorphisedTypes
          fGenerics
          (void . fst . getOuterAnnotation <$> args)
          fArgTypes
  newFuncs <- traverse (createDropFunction 1 . snd) types
  dropArgs <- fmap WFunctionPointer <$> traverse addGeneratedFunction newFuncs
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
