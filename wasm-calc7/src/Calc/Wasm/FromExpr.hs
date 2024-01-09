{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Calc.Wasm.FromExpr (fromModule) where

import           Calc.ExprUtils
import           Calc.Types
import           Calc.Wasm.Helpers
import           Calc.Wasm.Patterns
import           Calc.Wasm.Types
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.List            as List
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map.Strict      as M
import           GHC.Natural

-- | take our regular module and do the book keeping to get it ready for Wasm
-- town
data FromExprState = FromExprState
  { fesFunctions :: M.Map FunctionName FromExprFunc,
    fesImports   :: M.Map FunctionName FromExprImport,
    fesVars      :: [(Maybe Identifier, WasmType)]
  }

data FromExprFunc = FromExprFunc
  { fefIndex      :: Natural,
    fefArgs       :: [WasmType],
    fefReturnType :: WasmType
  }

newtype FromExprImport = FromExprImport { feiIndex :: Natural }

-- | add a local type, returning a unique index
addLocal ::
  (MonadState FromExprState m) =>
  Maybe Identifier ->
  WasmType ->
  m Natural
addLocal maybeIdent ty = do
  modify
    ( \fes ->
        fes
          { fesVars =
              fesVars fes <> [(maybeIdent, ty)]
          }
    )
  len <-
    gets
      ( fromIntegral . length . fesVars
      )
  pure (len - 1)

lookupIdent ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  Identifier ->
  m Natural
lookupIdent ident = do
  let matchIdent (_, (thisIdent, _)) = thisIdent == Just ident

  maybeNat <-
    gets
      ( List.find matchIdent
          . zip [0 ..]
          . fesVars
      )
  case maybeNat of
    Just (nat, _) -> pure nat
    Nothing       -> throwError $ IdentifierNotFound ident

lookupFunction ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  FunctionName ->
  m Natural
lookupFunction functionName = do
  maybeFunc <- gets (M.lookup functionName . fesFunctions)
  case maybeFunc of
    Just (FromExprFunc { fefIndex}) -> pure fefIndex
    Nothing  -> do
      maybeImport <- gets (M.lookup functionName . fesImports)
      case maybeImport of
        Just (FromExprImport { feiIndex }) -> pure feiIndex
        Nothing ->
          throwError $ FunctionNotFound functionName

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
    then WSequence <$> fromExpr expr <*> fromExpr rest
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

      -- `let i = <expr>; let a = i.1; let b = i.2; <rest>....`
      pure $
        WLet index wasmExpr $
          foldr
            ( \(bindingIndex, fetchExpr) thisExpr ->
                WLet bindingIndex fetchExpr thisExpr
            )
            wasmRest
            indexes

fromExpr ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m,
    Show ann
  ) =>
  Expr (Type ann) ->
  m WasmExpr
fromExpr (EPrim _ prim) = do
  pure (WPrim prim)
fromExpr (ELet _ pat expr rest) =
  fromLet pat expr rest
fromExpr (EInfix _ op a b) = do
  -- we're assuming that the types of `a` and `b` are the same
  -- we want the type of the args, not the result
  scalar <- liftEither $ scalarFromType (getOuterAnnotation a)
  WInfix scalar op <$> fromExpr a <*> fromExpr b
fromExpr (EIf _ predE thenE elseE) =
  WIf <$> fromExpr predE <*> fromExpr thenE <*> fromExpr elseE
fromExpr (EVar _ ident) =
  WVar <$> lookupIdent ident
fromExpr (EApply _ funcName args) = do
  fefIndex <- lookupFunction funcName
  WApply fefIndex
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
fromExpr (EContainerAccess ty tup nat) =
  let offset = getOffsetList (getOuterAnnotation tup) !! fromIntegral (nat - 1)
   in WTupleAccess
        <$> liftEither (scalarFromType ty)
        <*> fromExpr tup
        <*> pure offset
fromExpr (EBox ty inner) = do
  innerWasmType <- liftEither $ scalarFromType (getOuterAnnotation inner)
  containerWasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing containerWasmType
  boxed index innerWasmType <$> fromExpr inner

fromImport :: Import (Type ann) -> Either FromWasmError WasmImport
fromImport (Import {impReturnType, impExternalModule = Identifier wiExternalModule,
    impExternalFunction = Identifier wiExternalFunction, impImportName,impArgs}) = do
  args <-
    traverse
      ( \(ImportArg {iaName = ident, iaType}) -> do
          wasmType <- scalarFromType iaType
          pure (Just ident, wasmType)
      )
      impArgs

  wiReturnType <- scalarFromType impReturnType

  pure $ WasmImport {wiName=impImportName,
        wiArgs = snd <$> args,
                    wiExternalModule  ,
                    wiExternalFunction , wiReturnType }

fromFunction ::
  (Show ann) =>
  M.Map FunctionName FromExprFunc ->
  M.Map FunctionName FromExprImport ->
  Function (Type ann) ->
  Either FromWasmError WasmFunction
fromFunction funcMap importMap (Function {fnBody, fnArgs, fnFunctionName}) = do

  args <-
    traverse
      ( \(FunctionArg {faName = ArgumentName ident, faType}) -> do
          wasmType <- scalarFromType faType
          pure (Just (Identifier ident), wasmType)
      )
      fnArgs

  (expr, fes) <-
    runStateT
      (fromExpr fnBody)
      ( FromExprState
          { fesVars = args,
            fesImports = importMap,
            fesFunctions = funcMap
          }
      )

  retType <- scalarFromType (getOuterAnnotation fnBody)

  pure $
    WasmFunction
      { wfName = fnFunctionName,
        wfExpr = expr,
        wfPublic = False,
        wfArgs = snd <$> args,
        wfReturnType = retType,
        wfLocals = snd <$> fesVars fes
      }

-- take only the function info we need
getFunctionMap :: [Function (Type ann)] ->
  Either FromWasmError
    (M.Map FunctionName FromExprFunc)
getFunctionMap mdFunctions =
  M.fromList
    <$> traverse
      ( \(i, Function {fnFunctionName, fnArgs, fnBody}) -> do
          fefArgs <- traverse (scalarFromType . faType) fnArgs
          fefReturnType <- scalarFromType (getOuterAnnotation fnBody)
          pure
            ( fnFunctionName,
              FromExprFunc {fefIndex = i + 1, fefArgs, fefReturnType}
            )
      )
      (zip [0 ..] mdFunctions)

-- take only the function info we need
getImportMap :: Natural -> [Import (Type ann)] ->
  Either FromWasmError
    (M.Map FunctionName FromExprImport)
getImportMap offset mdImports =
  M.fromList
    <$> traverse
      ( \(i, Import {impImportName}) -> do
          pure
            ( impImportName,
              FromExprImport {feiIndex = i + 1}
            )
      )
      (zip [offset ..] mdImports)


fromModule ::
  (Show ann) =>
  Module (Type ann) ->
  Either FromWasmError WasmModule
fromModule (Module {mdExpr, mdImports,mdFunctions}) = do
  funcMap <- getFunctionMap mdFunctions
  importMap <- getImportMap (fromIntegral $ length funcMap) mdImports

  (expr, fes) <-
    runStateT
      (fromExpr mdExpr)
      ( FromExprState
          { fesVars = mempty,
            fesImports = importMap,
            fesFunctions = funcMap
          }
      )

  retType <- scalarFromType (getOuterAnnotation mdExpr)

  let mainFunction =
        WasmFunction
          { wfName = "main",
            wfExpr = expr,
            wfPublic = True,
            wfArgs = mempty,
            wfReturnType = retType,
            wfLocals = snd <$> fesVars fes
          }

  wasmFunctions <- traverse (fromFunction funcMap importMap) mdFunctions

  wasmImports <- traverse fromImport mdImports

  pure $
    WasmModule
      { wmFunctions = mainFunction : wasmFunctions,
        wmImports = wasmImports
      }
