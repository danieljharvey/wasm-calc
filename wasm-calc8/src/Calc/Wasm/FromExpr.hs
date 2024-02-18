{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Wasm.FromExpr (fromModule) where

import Calc.ExprUtils
import Calc.Types
import Calc.Wasm.Helpers
import Calc.Wasm.Patterns
import Calc.Wasm.Types
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import GHC.Natural

-- | take our regular module and do the book keeping to get it ready for Wasm
-- town
data FromExprState = FromExprState
  { fesFunctions :: M.Map FunctionName FromExprFunc,
    fesImports :: M.Map FunctionName FromExprImport,
    fesGlobals :: M.Map Identifier FromExprGlobal,
    fesVars :: [(Maybe Identifier, WasmType)],
    fesArgs :: [(Identifier, WasmType)]
  }
  deriving stock (Eq, Ord, Show)

data FromExprGlobal = FromExprGlobal
  {fegIndex :: Natural, fegExpr :: WasmExpr, fegType :: WasmType}
  deriving stock (Eq, Ord, Show)

data FromExprFunc = FromExprFunc
  { fefIndex :: Natural,
    fefArgs :: [WasmType],
    fefReturnType :: WasmType
  }
  deriving stock (Eq, Ord, Show)

newtype FromExprImport = FromExprImport {feiIndex :: Natural}
  deriving newtype (Eq, Ord, Show)

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

  varLen <-
    gets
      ( fromIntegral . length . fesVars
      )

  argLen <-
    gets (fromIntegral . length . fesArgs)

  pure (argLen + varLen - 1)

lookupGlobal ::
  ( MonadError FromWasmError m,
    MonadState FromExprState m
  ) =>
  Identifier ->
  m Natural
lookupGlobal ident = do
  maybeGlobalNat <-
    gets
      ( M.lookup ident
          . fesGlobals
      )

  case maybeGlobalNat of
    Just (FromExprGlobal {fegIndex}) -> pure fegIndex
    Nothing ->
      throwError $ IdentifierNotFound ident

lookupIdent ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  Identifier ->
  m Natural
lookupIdent ident = do
  let matchVarIdent (_, (thisIdent, _)) = thisIdent == Just ident
      matchArgIdent (_, (thisIdent, _)) = thisIdent == ident

  startingDigit <- gets (fromIntegral . length . fesArgs)

  maybeVarNat <-
    gets
      ( List.find matchVarIdent
          . zip [startingDigit ..]
          . fesVars
      )
  case maybeVarNat of
    Just (nat, _) -> pure nat
    Nothing -> do
      -- check in args
      maybeArgNat <-
        gets
          ( List.find matchArgIdent
              . zip [0 ..]
              . fesArgs
          )
      case maybeArgNat of
        Just (nat, _) -> pure nat
        Nothing ->
          throwError $ IdentifierNotFound ident

-- | user defined functions live after any imports, and our alloc is the first
-- function
getAllocationFunctionNumber :: (MonadState FromExprState m) => m Natural
getAllocationFunctionNumber =
  gets (fromIntegral . length . fesImports)

lookupFunction ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  FunctionName ->
  m Natural
lookupFunction functionName = do
  maybeFunc <- gets (M.lookup functionName . fesFunctions)
  case maybeFunc of
    Just (FromExprFunc {fefIndex}) -> pure fefIndex
    Nothing -> do
      maybeImport <- gets (M.lookup functionName . fesImports)
      case maybeImport of
        Just (FromExprImport {feiIndex}) -> pure feiIndex
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

      -- `let i = <expr>; let a = i.1; let b = i.2; <rest>....`
      pure $
        WLet index wasmExpr $
          foldr
            ( \(bindingIndex, fetchExpr) thisExpr ->
                WLet bindingIndex fetchExpr thisExpr
            )
            wasmRest
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
  fefIndex <- lookupFunction funcName
  WApply fefIndex
    <$> traverse fromExpr args
fromExpr (ETuple ty a as) = do
  wasmType <- liftEither $ scalarFromType ty
  index <- addLocal Nothing wasmType
  fnIndex <- getAllocationFunctionNumber
  let allItems = zip [0 ..] (a : NE.toList as)
      tupleLength = memorySizeForType ty
      allocate = WAllocate fnIndex (fromIntegral tupleLength)
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
  fnIndex <- getAllocationFunctionNumber
  boxed fnIndex index innerWasmType <$> fromExpr inner
fromExpr (ELoad ty index) = do
  wasmType <- liftEither $ scalarFromType ty
  pure $ WLoad wasmType (fromIntegral index)
fromExpr (EStore _ index expr) = do
  wasmType <- liftEither $ scalarFromType (getOuterAnnotation expr)
  WStore wasmType (fromIntegral index) <$> fromExpr expr

fromImport :: Import (Type ann) -> Either FromWasmError WasmImport
fromImport
  ( Import
      { impReturnType,
        impExternalModule = Identifier wiExternalModule,
        impExternalFunction = Identifier wiExternalFunction,
        impImportName,
        impArgs
      }
    ) = do
    args <-
      traverse
        ( \(ImportArg {iaName = ident, iaType}) -> do
            wasmType <- scalarFromType iaType
            pure (Just ident, wasmType)
        )
        impArgs

    wiReturnType <- scalarFromType impReturnType

    pure $
      WasmImport
        { wiName = impImportName,
          wiArgs = snd <$> args,
          wiExternalModule,
          wiExternalFunction,
          wiReturnType
        }

fromFunction ::
  (Show ann) =>
  M.Map FunctionName FromExprFunc ->
  M.Map FunctionName FromExprImport ->
  M.Map Identifier FromExprGlobal ->
  Function (Type ann) ->
  Either FromWasmError WasmFunction
fromFunction funcMap importMap globalMap (Function {fnPublic, fnBody, fnArgs, fnFunctionName}) = do
  args <-
    traverse
      ( \(FunctionArg {faName = ArgumentName ident, faType}) -> do
          wasmType <- scalarFromType faType
          pure (Identifier ident, wasmType)
      )
      fnArgs

  (expr, fes) <-
    runStateT
      (fromExpr fnBody)
      ( FromExprState
          { fesVars = mempty,
            fesArgs = args,
            fesGlobals = globalMap,
            fesImports = importMap,
            fesFunctions = funcMap
          }
      )

  retType <- scalarFromType (getOuterAnnotation fnBody)

  pure $
    WasmFunction
      { wfName = fnFunctionName,
        wfExpr = expr,
        wfPublic = fnPublic,
        wfArgs = snd <$> args,
        wfReturnType = retType,
        wfLocals = snd <$> fesVars fes
      }

-- take only the information about globals that we need
-- we assume each global uses no imports or functions
getGlobalMap ::
  (Show ann) =>
  [Global (Type ann)] ->
  Either FromWasmError (M.Map Identifier FromExprGlobal)
getGlobalMap globals =
  M.fromList
    <$> traverse
      ( \(fegIndex, Global {glbIdentifier, glbExpr}) -> do
          (fegExpr, _) <-
            runStateT
              (fromExpr glbExpr)
              ( FromExprState
                  { fesVars = mempty,
                    fesArgs = mempty,
                    fesGlobals = mempty,
                    fesImports = mempty,
                    fesFunctions = mempty
                  }
              )

          fegType <- scalarFromType (getOuterAnnotation glbExpr)
          pure (glbIdentifier, FromExprGlobal {fegExpr, fegIndex, fegType})
      )
      (zip [0 ..] globals)

-- take only the function info we need
getFunctionMap ::
  Natural ->
  [Function (Type ann)] ->
  Either
    FromWasmError
    (M.Map FunctionName FromExprFunc)
getFunctionMap offset mdFunctions =
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
      (zip [offset ..] mdFunctions)

-- take only the function info we need
getImportMap ::
  [Import (Type ann)] ->
  Either
    FromWasmError
    (M.Map FunctionName FromExprImport)
getImportMap mdImports =
  M.fromList
    <$> traverse
      ( \(i, Import {impImportName}) -> do
          pure
            ( impImportName,
              FromExprImport {feiIndex = i}
            )
      )
      (zip [0 ..] mdImports)

fromMemory :: Maybe (Memory (Type ann)) -> WasmMemory
fromMemory Nothing = WasmMemory 0 Nothing
fromMemory (Just (LocalMemory {lmLimit})) =
  WasmMemory lmLimit Nothing
fromMemory
  ( Just
      ( ImportedMemory
          { imExternalModule = Identifier imExternalModule,
            imExternalMemoryName = Identifier imExternalMemoryName,
            imLimit
          }
        )
    ) =
    WasmMemory imLimit (Just (imExternalModule, imExternalMemoryName))

fromGlobal :: (Show ann) => Global (Type ann) -> Either FromWasmError WasmGlobal
fromGlobal (Global {glbExpr}) = do
  (wgExpr, _) <-
    runStateT
      (fromExpr glbExpr)
      ( FromExprState
          { fesVars = mempty,
            fesArgs = mempty,
            fesGlobals = mempty,
            fesImports = mempty,
            fesFunctions = mempty
          }
      )

  wgType <- scalarFromType (getOuterAnnotation glbExpr)
  pure $ WasmGlobal {wgExpr, wgType}

fromModule ::
  (Show ann) =>
  Module (Type ann) ->
  Either FromWasmError WasmModule
fromModule (Module {mdMemory, mdGlobals, mdImports, mdFunctions}) = do
  importMap <- getImportMap mdImports
  funcMap <- getFunctionMap (fromIntegral (length importMap)) mdFunctions
  globalMap <- getGlobalMap mdGlobals

  wasmGlobals <- traverse fromGlobal mdGlobals

  wasmFunctions <-
    traverse
      (fromFunction funcMap importMap globalMap)
      mdFunctions

  wasmImports <- traverse fromImport mdImports

  pure $
    WasmModule
      { wmFunctions = wasmFunctions,
        wmImports = wasmImports,
        wmMemory = fromMemory mdMemory,
        wmGlobals = wasmGlobals
      }
