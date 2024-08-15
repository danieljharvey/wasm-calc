{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Wasm.FromExpr.Helpers
  ( getAbilitiesForFunction,
    scalarFromType,
    lookupDataType,
    addLocal,
    lookupGlobal,
    lookupIdent,
    addGeneratedFunction,
    getGlobalMap,
    getFunctionMap,
    getImportMap,
    lookupFunction,
    genericArgName,
    monomorphiseTypes,
    fromPrim,
    getOffsetList,
    getOffsetListForConstructor,
    boxed,
    memorySizeForType,
    getConstructorNumber,
  )
where

import Calc.ExprUtils
import Calc.Typecheck
  ( TypecheckEnv (..),
    runTypecheckM,
  )
import Calc.Typecheck.Helpers (calculateMonomorphisedTypes)
import Calc.Types
import Calc.Wasm.FromExpr.Types
import Calc.Wasm.ToWasm.Types
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Natural

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

lookupFunction ::
  (MonadState FromExprState m, MonadError FromWasmError m) =>
  FunctionName ->
  m (WasmFunctionRef, [TypeVar], [Type ()])
lookupFunction functionName = do
  maybeFunc <- gets (M.lookup functionName . fesFunctions)
  case maybeFunc of
    Just (FromExprFunc {fefIndex, fefGenerics, fefOriginalArgs}) ->
      pure (WasmFunctionRef fefIndex, fefGenerics, fefOriginalArgs)
    Nothing -> do
      maybeImport <- gets (M.lookup functionName . fesImports)
      case maybeImport of
        Just (FromExprImport {feiIndex}) -> pure (WasmImportRef feiIndex, mempty, mempty)
        Nothing ->
          throwError $ FunctionNotFound functionName

addGeneratedFunction :: (MonadState FromExprState m) => WasmFunction -> m WasmFunctionRef
addGeneratedFunction wasmFunc = do
  modify (\fes -> fes {fesGenerated = fesGenerated fes <> [wasmFunc]})
  startingDigit <- gets (fromIntegral . length . fesGenerated)
  pure (WasmGeneratedRef $ startingDigit - 1)

-- take only the information about globals that we need
-- we assume each global uses no imports or functions
getGlobalMap ::
  [Global (Type ann)] ->
  Either FromWasmError (M.Map Identifier FromExprGlobal)
getGlobalMap globals =
  M.fromList
    <$> traverse
      ( \(fegIndex, Global {glbIdentifier}) -> do
          pure (glbIdentifier, FromExprGlobal {fegIndex})
      )
      (zip [0 ..] globals)

getAbilitiesForFunction :: M.Map FunctionName (S.Set (Ability ann)) -> FunctionName -> Either FromWasmError (S.Set (Ability ann))
getAbilitiesForFunction functionAbilities fnName =
  case M.lookup fnName functionAbilities of
    Just a -> pure a
    Nothing -> throwError (FunctionAbilityLookupFailed fnName)

-- take only the function info we need
getFunctionMap ::
  [Function (Type ann)] ->
  Either
    FromWasmError
    (M.Map FunctionName FromExprFunc)
getFunctionMap mdFunctions =
  M.fromList
    <$> traverse
      ( \(i, Function {fnFunctionName, fnGenerics, fnArgs, fnBody}) -> do
          fefArgs <- traverse (scalarFromType . faType) fnArgs
          fefReturnType <- scalarFromType (getOuterAnnotation fnBody)
          pure
            ( fnFunctionName,
              FromExprFunc
                { fefIndex = i,
                  fefArgs,
                  fefOriginalArgs = void . faType <$> fnArgs,
                  fefReturnType,
                  fefGenerics = fnGenerics
                }
            )
      )
      (zip [0 ..] mdFunctions)

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

scalarFromType :: Type ann -> Either FromWasmError WasmType
scalarFromType (TPrim _ TVoid) = pure Void
scalarFromType (TPrim _ TBool) = pure I32
scalarFromType (TPrim _ TInt8) = pure I8
scalarFromType (TPrim _ TInt16) = pure I16
scalarFromType (TPrim _ TInt32) = pure I32
scalarFromType (TPrim _ TInt64) = pure I64
scalarFromType (TPrim _ TFloat32) = pure F32
scalarFromType (TPrim _ TFloat64) = pure F64
scalarFromType (TFunction {}) = Left FunctionTypeNotScalar
scalarFromType (TContainer {}) = pure Pointer
scalarFromType (TVar _ _) =
  pure Pointer -- all polymorphic variables are Pointer
scalarFromType (TUnificationVar {}) =
  pure Pointer
scalarFromType (TConstructor {}) = pure Pointer -- maybe enums will become I8 in future, but for now, it's all pointers

genericArgName :: TypeVar -> Identifier
genericArgName generic =
  Identifier $ "generic_" <> T.pack (show generic)

-- if we run `fn thing<a,b>(one:a, two: b)` as `thing((1:Int32), (2: Int64))`
-- then we know `a == Int32` and `b == Int64`.
monomorphiseTypes ::
  (Show ann) =>
  [TypeVar] ->
  [Type ann] ->
  [Type ann] ->
  [(TypeVar, Type ann)]
monomorphiseTypes typeVars fnArgTys argTys =
  let tcEnv =
        TypecheckEnv
          { tceVars = mempty,
            tceGenerics = mempty,
            tceMemoryLimit = 0,
            tceDataTypes = mempty
          }
   in case runTypecheckM tcEnv (calculateMonomorphisedTypes typeVars fnArgTys argTys mempty) of
        Right tvs -> tvs
        Left e -> error (show e)

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

getOffsetList :: Type ann -> [Natural]
getOffsetList (TContainer _ items) =
  scanl (\offset item -> offset + offsetForType item) 0 (NE.toList items)
getOffsetList _ = []

lookupDataType :: (MonadState FromExprState m) => DataName -> m (Data ())
lookupDataType dataTypeName = do
  maybeDataType <- gets (M.lookup dataTypeName . fesDataTypes)
  case maybeDataType of
    Just dt -> pure dt
    Nothing -> error $ "oh fuck couldn't find " <> show dataTypeName

getOffsetListForConstructor ::
  (MonadError FromWasmError m, MonadState FromExprState m) =>
  Type ann ->
  Constructor ->
  m [Natural]
getOffsetListForConstructor (TConstructor _ dataTypeName tyItems) constructor = do
  (Data _ dtVars constructors) <- lookupDataType dataTypeName
  wasmTys <- case M.lookup constructor constructors of
    Just constructorTys -> do
      -- for `Just(I32)`, replace `a` in Maybe<a> with I32
      let replacements = M.fromList $ monomorphiseTypes dtVars constructorTys (void <$> tyItems)

      -- now go through `tyItems`, swapping out TVar with `replacements`
      -- then finally, run `memorySizeForType` on everything

      -- now we've learned about the types, swap the polymorphic ones for the
      -- monomorphised ones
      let toWasm ty = case ty of
            TVar _ identifier -> case M.lookup identifier replacements of
              Just a -> liftEither (scalarFromType a)
              Nothing -> pure Pointer -- polymorphic values become "Pointer", this seems boringly safe
            other -> liftEither (scalarFromType other)

      traverse toWasm constructorTys
    Nothing ->
      error $ "did not find constructor " <> show constructor
  pure $ scanl (\offset item -> offset + memorySize item) (memorySize I8) wasmTys
getOffsetListForConstructor _ _ = pure []

-- 1 item is a byte, so i8, so i32 is 4 bytes
memorySize :: WasmType -> Natural
memorySize I8 = 1
memorySize I16 = 2
memorySize I32 = 4
memorySize I64 = 8
memorySize F32 = 4
memorySize F64 = 8
memorySize Pointer = memorySize I32
memorySize Void = 0

-- | wrap a `WasmExpr` in a single item struct
boxed :: Natural -> WasmType -> WasmExpr -> WasmExpr
boxed index ty wExpr =
  let allocate = WAllocate (memorySize ty)
   in WSet index allocate [(0, ty, wExpr)]

-- | size of the primitive in memory (ie, struct is size of its pointer)
offsetForType :: Type ann -> Natural
offsetForType (TPrim _ TInt8) =
  memorySize I8
offsetForType (TPrim _ TInt16) =
  memorySize I16
offsetForType (TPrim _ TInt32) =
  memorySize I32
offsetForType (TPrim _ TInt64) =
  memorySize I64
offsetForType (TPrim _ TFloat32) =
  memorySize F32
offsetForType (TPrim _ TFloat64) =
  memorySize F64
offsetForType (TPrim _ TBool) =
  memorySize I32
offsetForType (TConstructor {}) =
  memorySize Pointer
offsetForType (TPrim _ TVoid) =
  error "offsetForType TVoid"
offsetForType (TContainer _ _) =
  memorySize Pointer
offsetForType (TFunction {}) =
  memorySize Pointer
offsetForType (TVar _ _) =
  memorySize Pointer
offsetForType (TUnificationVar _ _) =
  error "offsetForType TUnificationVar"

-- | the actual size of the item in memory
-- | for sum types this will be the biggest possible amount
memorySizeForType :: (MonadState FromExprState m) => Type ann -> m Natural
memorySizeForType (TPrim _ TInt8) = pure $ memorySize I8
memorySizeForType (TPrim _ TInt16) =
  pure $ memorySize I16
memorySizeForType (TPrim _ TInt32) =
  pure $ memorySize I32
memorySizeForType (TPrim _ TInt64) =
  pure $ memorySize I64
memorySizeForType (TPrim _ TFloat32) =
  pure $ memorySize F32
memorySizeForType (TPrim _ TFloat64) =
  pure $ memorySize F64
memorySizeForType (TPrim _ TBool) =
  pure $ memorySize I32
memorySizeForType (TPrim _ TVoid) =
  error "memorySizeForType TVoid"
memorySizeForType (TConstructor _ dataTypeName _) = do
  (Data _ _ constructors) <- lookupDataType dataTypeName
  let discriminator = memorySize I8
      sizeOfConstructor tys =
        getSum <$> (mconcat <$> traverse (fmap Sum . memorySizeInsideConstructor) tys)
  sizes <- traverse sizeOfConstructor (M.elems constructors)
  pure $ discriminator + maximum sizes
memorySizeForType (TContainer _ as) =
  getSum <$> (mconcat <$> traverse (fmap Sum . memorySizeInsideConstructor) (NE.toList as))
memorySizeForType (TFunction {}) =
  pure $ memorySize Pointer
memorySizeForType (TVar _ _) =
  pure $ memorySize Pointer
memorySizeForType (TUnificationVar _ _) =
  error "memorySizeForType TUnificationVar"

-- nested data types only take up "Pointer"
memorySizeInsideConstructor :: (MonadState FromExprState m) => Type ann -> m Natural
memorySizeInsideConstructor (TContainer {}) = pure $ memorySize Pointer
memorySizeInsideConstructor (TConstructor {}) = pure $ memorySize Pointer
memorySizeInsideConstructor other = memorySizeForType other

getConstructorNumber :: (MonadState FromExprState m) => Type ann -> Constructor -> m Natural
getConstructorNumber ty constructor = do
  (Data _ _ constructors) <- case ty of
    TConstructor _ dataTypeName _ -> lookupDataType dataTypeName
    _ -> error $ "expected TConstructor, got " <> show (void ty)
  let numberMap = M.fromList $ zip (M.keys constructors) [0 ..]
  case M.lookup constructor numberMap of
    Just nat -> pure nat
    Nothing -> error $ "constructor not found " <> show constructor
