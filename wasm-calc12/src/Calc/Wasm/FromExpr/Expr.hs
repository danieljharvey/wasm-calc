{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.FromExpr.Expr (fromExpr) where

import           Calc.ExprUtils
import           Calc.Linearity              (Drops (..))
import           Calc.Types
import           Calc.Wasm.FromExpr.Drops    (addDropsFromPath,
                                              addDropsToWasmExpr,
                                              dropFunctionForType,
                                              dropInstructionForType)
import           Calc.Wasm.FromExpr.Helpers
import           Calc.Wasm.FromExpr.Patterns
import           Calc.Wasm.FromExpr.Types
import           Calc.Wasm.ToWasm.Types
import           Control.Monad               (void)
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import           GHC.Natural

patternBindings ::
  (MonadError FromWasmError m, MonadState FromExprState m, Show ann, Eq ann) =>
  Pattern (Type ann, Maybe (Drops ann)) ->
  Expr (Type ann, Maybe (Drops ann)) ->
  Natural ->
  m WasmExpr
patternBindings pat patExpr index = do
  paths <- patternToPaths (fst <$> pat) id

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
          pure (ident, bindingIndex, fetchExpr)
      )
      (M.toList paths)

  -- convert the continuation expr
  wasmPatExpr <- fromExprWithDrops patExpr

  dropPaths <- patternToDropPaths pat id

  -- drop items in the match expr we will no longer need
  dropPathExprs <-
    traverse (addDropsFromPath index) dropPaths

  -- take care of stuff we've pattern matched into oblivion
  let wasmPatExprWithDrops = foldr (WSequence Void) wasmPatExpr dropPathExprs

  pure $
    foldr
      ( \(ident, bindingIndex, fetchExpr) thisExpr ->
          WLet (Just ident) bindingIndex fetchExpr thisExpr
      )
      wasmPatExprWithDrops
      indexes

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
  paths <- patternToPaths (fst <$> pat) id
  if null paths
    then do
      wasmTy <- liftEither $ scalarFromType $ fst $ getOuterPatternAnnotation pat
      WSequence wasmTy <$> fromExpr expr <*> fromExpr rest
    else do
      -- get type of the main expr
      wasmType <- liftEither $ scalarFromType $ fst $ getOuterAnnotation expr

      -- convert expr
      wasmExpr <- fromExpr expr

      -- if the matching expr isn't already a variable,
      -- we make a nameless binding of the whole value
      case wasmExpr of
        WVar i -> patternBindings pat rest i
        _ -> do
          index <- addLocal Nothing wasmType

          -- convert rest of pattern match
          wasmRest <- patternBindings pat rest index

          -- `let i = <expr>; let a = i.1; let b = i.2; <rest>....`
          pure $ WLet Nothing index wasmExpr wasmRest

fromMatch ::
  (MonadState FromExprState m, MonadError FromWasmError m, Eq ann, Show ann) =>
  Expr (Type ann, Maybe (Drops ann)) ->
  NE.NonEmpty
    ( Pattern (Type ann, Maybe (Drops ann)),
      Expr (Type ann, Maybe (Drops ann))
    ) ->
  m WasmExpr
fromMatch expr pats = do
  let (headPat, headExpr) = NE.head pats
  case NE.nonEmpty (NE.tail pats) of
    Nothing -> do
      -- single match, use the `let` code
      fromLet headPat expr headExpr
    Just _nePats -> do
      -- actual multiple patterns

      -- get type of the main expr
      wasmType <- liftEither $ scalarFromType $ fst $ getOuterAnnotation expr

      -- convert expr
      wasmExpr <- fromExpr expr

      -- if the matching expr isn't already a variable,
      -- we make a nameless binding of the whole value
      (needsLet, index) <- case wasmExpr of
        WVar i -> pure (False, i)
        _      -> (,) True <$> addLocal Nothing wasmType

      -- return type of exprs
      wasmReturnType <- liftEither $ scalarFromType $ fst $ getOuterAnnotation headExpr

      -- fold through patterns
      wasmPatExpr <-
        foldr
          ( \(pat, patExpr) wholeExpr -> do
              preds <-
                predicatesFromPattern (fst <$> pat) mempty
              predExprs <-
                traverse (predicateToWasm (WVar index)) preds
              wasmPatExpr <- patternBindings pat patExpr index
              case NE.nonEmpty predExprs of
                Nothing -> pure wasmPatExpr
                Just neExprs ->
                  WIf wasmReturnType (andExprs neExprs) wasmPatExpr <$> wholeExpr
          )
          (pure WUnreachable)
          (NE.toList pats)

      -- let's not make more temporary bindings we need, for the sake of
      -- reading our IR
      if needsLet
        then -- `let i = <expr>; let a = i.1; let b = i.2; <rest>....`
          pure $ WLet Nothing index wasmExpr wasmPatExpr
        else pure wasmPatExpr

andExprs :: NE.NonEmpty WasmExpr -> WasmExpr
andExprs neExprs =
  foldr (WInfix I32 OpAnd) (NE.head neExprs) (NE.tail neExprs)

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

allVars :: Expr (Type ann, Maybe (Drops ann)) -> S.Set Identifier
allVars =
  go
  where
    go (EVar _ _ ident) = S.singleton ident
    go other            = monoidExpr go other

fromLambda ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann,
    Eq ann
  ) =>
  [(Identifier, Type (Type ann, Maybe (Drops ann)))] ->
  Type (Type ann, Maybe (Drops ann)) ->
  Expr (Type ann, Maybe (Drops ann)) ->
  m WasmExpr
fromLambda args returnTy body = do
  -- can we do capture here?

  let capturedIdentifiers = S.difference (allVars body) (S.fromList (fst <$> args))

  capturedArgs <-
    traverse
      (\k -> (,) k <$> (snd <$> lookupIdent k))
      (S.toList capturedIdentifiers)

  capturedValues <-
    traverse
      ( \k -> do
          (index, wasmTy) <- lookupIdent k
          pure (wasmTy, WVar index)
      )
      (S.toList capturedIdentifiers)

  wasmArgs <-
    traverse (\(k, a) -> (,) k <$> liftEither (scalarFromType a)) args

  let wasmItems = snd <$> capturedArgs
      offsetList = getOffsetListForWasmType wasmItems
      allArgs = wasmArgs <> [("_env", Pointer)]
      moreArgs = allArgs <> capturedArgs

  (wasmBody, lambdaLocals) <- withArgs moreArgs (fromExpr body)

  let envOffset = fromIntegral $ length wasmArgs

  -- destructure all the env into vars again
  let wasmBodyWithGetters =
        foldr
          ( \(i, (identifier, wasmTy)) wasmExpr' ->
              WLet
                (Just identifier)
                (fromIntegral $ envOffset + i + 1)
                ( WTupleAccess
                    wasmTy
                    (WVar envOffset)
                    (offsetList !! fromIntegral i)
                )
                wasmExpr'
          )
          wasmBody
          (zip [0 ..] capturedArgs)

  wasmReturnType <- liftEither $ scalarFromType returnTy

  index <- gets (length . fesGenerated)

  -- create function
  let fn =
        WasmFunction
          { wfName = FunctionName ("fresh_lambda_" <> T.pack (show index)),
            wfExpr = wasmBodyWithGetters,
            wfPublic = False,
            wfArgs = snd <$> allArgs,
            wfReturnType = wasmReturnType,
            wfLocals = (snd <$> lambdaLocals) <> (snd <$> capturedArgs), -- captured args will be destructed as vars
            wfAbilities = mempty
          }

  -- store it in heaven
  wasmFnRef <- addGeneratedFunction fn

  -- first, create a tuple of [capturedArgA, capturedArgB, .. ]
  envIndex <- addLocal Nothing Pointer

  -- total size of the tuple in memory
  let envTupleLength = getMemorySizeForWasmTuple wasmItems

  wasmEnv <-
    WSet envIndex (WAllocate envTupleLength)
      <$> traverse
        ( \(i, (wasmTy, wasmItem)) ->
            pure
              ( (,,)
                  (offsetList !! i)
                  wasmTy
                  wasmItem
              )
        )
        (zip [0 ..] capturedValues)

  -- then we create a tuple of [WFunctionPointer, pointerToEnv]
  -- and return it
  allocIndex <- addLocal Nothing Pointer

  -- total size of the tuple in memory
  let tupleLength = getMemorySizeForWasmTuple [Pointer, Pointer]

  let wSet =
        WSet
          allocIndex
          (WAllocate tupleLength)
          [ (0, Pointer, WFunctionPointer wasmFnRef),
            (memorySize Pointer, Pointer, wasmEnv)
          ]

  pure wSet

fromConstructor ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann,
    Eq ann
  ) =>
  Type ann ->
  Constructor ->
  [Expr (Type ann, Maybe (Drops ann))] ->
  m WasmExpr
fromConstructor ty constructor args = do
  -- what is the underlying discriminator value?
  constructorNumber <- fmap (WPrim . WPInt32 . fromIntegral) <$> getConstructorNumber ty constructor

  wasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing wasmType
  let allItems = zip [0 ..] args
  tupleLength <- memorySizeForType ty
  let allocate = WAllocate (fromIntegral tupleLength)
  offsetList <- getOffsetListForConstructor ty constructor

  wasmItems <-
    traverse
      ( \(i, item) ->
          (,,) (offsetList !! i)
            <$> liftEither (scalarFromType (fst $ getOuterAnnotation item))
            <*> fromExpr item
      )
      allItems

  let allWasmItems = case constructorNumber of
        Just nat -> (0, I8, nat) : wasmItems
        Nothing  -> wasmItems

  pure $ WSet index allocate allWasmItems

fromTuple ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann,
    Eq ann
  ) =>
  Type ann ->
  Expr (Type ann, Maybe (Drops ann)) ->
  NE.NonEmpty (Expr (Type ann, Maybe (Drops ann))) ->
  m WasmExpr
fromTuple ty a as = do
  wasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing wasmType
  let allItems = zip [0 ..] (a : NE.toList as)
  tupleLength <- memorySizeForType ty
  let allocate = WAllocate (fromIntegral tupleLength)
      offsetList = getOffsetList ty
  WSet index allocate
    <$> traverse
      ( \(i, item) ->
          (,,) (offsetList !! i)
            <$> liftEither (scalarFromType (fst $ getOuterAnnotation item))
            <*> fromExpr item
      )
      allItems

data FunctionApply = TopLevelFunction WasmExpr | Lambda WasmExpr

fromApply ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann,
    Eq ann
  ) =>
  Expr (Type ann, Maybe (Drops ann)) ->
  [Expr (Type ann, Maybe (Drops ann))] ->
  m WasmExpr
fromApply fnExpr args = do
  wasmFn <-
    ( Lambda
        <$> fromExpr fnExpr
      )
      `catchError` \_ ->
        case fnExpr of
          EVar _ _ (Identifier ident) -> do
            -- maybe it's a function
            (fIndex, fGenerics, fArgTypes) <- lookupFunction (FunctionName ident)
            let argTypes =
                  monomorphiseTypes
                    fGenerics
                    fArgTypes
                    (void . fst . getOuterAnnotation <$> args)
            dropArgs <- traverse (dropFunctionForType . snd) argTypes
            wasmArgs <- traverse fromExpr args
            let allArgs = wasmArgs <> dropArgs
            pure (TopLevelFunction (WApply fIndex allArgs))
          _ -> error "what"
  case wasmFn of
    TopLevelFunction wasmExpr -> pure wasmExpr
    Lambda fn -> do
      let ty = getOuterAnnotation fnExpr

      let returnType = case fst ty of
            TFunction _ _ ret -> ret
            _                 -> error "argggh"

      wasmReturnType <- liftEither $ scalarFromType returnType

      dropFn <- dropInstructionForType fn (fst ty)

      -- get the functions out of the tuple
      let wasmFunctionPointer = WTupleAccess Pointer fn 0

      -- sort the user provided args
      wasmArgs <- traverse fromExpr args

      let wasmEnv = WTupleAccess Pointer fn (memorySize Pointer)
      let allArgs = wasmArgs <> [wasmEnv]

      index <- addLocal Nothing wasmReturnType

      let wasm =
            WLet
              Nothing
              index
              (WApplyIndirect wasmFunctionPointer allArgs)
              (WSequence Void dropFn (WVar index))

      pure wasm

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
fromExpr (ELambda _ args returnTy body) = do
  fromLambda args returnTy body
fromExpr (EConstructor (ty, _) constructor args) = do
  fromConstructor ty constructor args
fromExpr (EBlock (_, Just _) _) = do
  error "found drops on block"
fromExpr (EBlock _ expr) = do
  fromExpr expr
fromExpr (EAnn (_, Just _) _ _) = do
  error "found drops on annotation"
fromExpr (EAnn _ _ expr) =
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
fromExpr (EVar _ _ ident) = do
  (WVar . fst <$> lookupIdent ident)
    `catchError` \_ -> WGlobal <$> lookupGlobal ident
fromExpr (EApply _ fnExpr args) = do
  fromApply fnExpr args
fromExpr (ETuple (ty, _) a as) = do
  fromTuple ty a as
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
