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
import           Control.Monad               (void)
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Strict             as M

fromLet ::
  ( Show ann,
    MonadError FromWasmError m,
    MonadState FromExprState m
  ) =>
  (Type ann, Maybe Drops) ->
  Pattern (Type ann, Maybe Drops) ->
  Expr (Type ann, Maybe Drops) ->
  Expr (Type ann, Maybe Drops) ->
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
      wasmRestWithDrops <- case drops of
        Just (DropIdentifiers idents) -> do
          nats <- traverse lookupIdent idents
          pure $
            foldr
              ( WSequence Void . WDrop . WVar
              )
              wasmRest
              nats
        Just DropMe -> pure wasmRest -- TODO: what does this mean? anything?
        Nothing -> pure wasmRest

      -- drop items in the match expr we will no longer need
      dropPaths <-
        traverse (fmap WDrop <$> fromPath index) (exprToPaths expr id)

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

fromExpr ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann
  ) =>
  Expr (Type ann, Maybe Drops) ->
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
  WIf wasmType <$> fromExpr predE <*> fromExpr thenE <*> fromExpr elseE
fromExpr (EVar _ ident) = do
  (WVar <$> lookupIdent ident)
    `catchError` \_ -> WGlobal <$> lookupGlobal ident
fromExpr (EApply _ funcName args) = do
  (fIndex, fGenerics, fArgTypes) <- lookupFunction funcName
  let types = calculateMonomorphisedTypes fGenerics
                  (void . fst . getOuterAnnotation <$> args)
                  fArgTypes
  newFuncs <- traverse ( createDropFunction 1 . snd) types
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
