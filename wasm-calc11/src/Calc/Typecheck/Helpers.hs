{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Typecheck.Helpers
  ( runTypecheckM,
    lookupVar,
    withVar,
    lookupFunction,
    withFunctionEnv,
    storeFunction,
    storeGlobal,
    lookupGlobal,
    arrangeDataTypes,
    calculateMonomorphisedTypes,
    lookupConstructor,
    matchConstructorTypesToArgs,
  )
where

import Calc.Typecheck.Error
import Calc.Typecheck.Generalise
import Calc.Typecheck.Types
import Calc.Typecheck.Unify
import Calc.Types
import Control.Monad (when, zipWithM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

-- | run a typechecking computation, discarding any Writer output
runTypecheckM ::
  TypecheckEnv ann ->
  TypecheckM ann a ->
  Either (TypeError ann) a
runTypecheckM env action =
  let typecheckState =
        TypecheckState
          { tcsFunctions = mempty,
            tcsGlobals = mempty,
            tcsUnique = 0,
            tcsUnified = mempty
          }
   in evalStateT
        ( runReaderT (getTypecheckM action) env
        )
        typecheckState

arrangeDataTypes :: [Data ann] -> M.Map Constructor (TCDataType ann)
arrangeDataTypes =
  foldMap
    ( \(Data {dtName, dtVars, dtConstructors}) ->
        let f (constructor, args) =
              M.singleton constructor (TCDataType dtName dtVars args)
         in foldMap f (M.toList dtConstructors)
    )

storeFunction ::
  FunctionName ->
  S.Set TypeVar ->
  Type ann ->
  TypecheckM ann ()
storeFunction fnName generics ty =
  modify
    ( \tcs ->
        tcs
          { tcsFunctions =
              HM.insert
                fnName
                (TypeScheme ty generics)
                (tcsFunctions tcs)
          }
    )

storeGlobal :: Identifier -> Mutability -> Type ann -> TypecheckM ann ()
storeGlobal ident mutable ty =
  modify
    ( \tcs ->
        tcs
          { tcsGlobals = HM.insert ident (TypecheckGlobal ty mutable) (tcsGlobals tcs)
          }
    )

-- | look up a saved identifier "in the environment"
lookupFunction :: ann -> FunctionName -> TypecheckM ann (Type ann)
lookupFunction ann fnName = do
  maybeType <- gets (HM.lookup fnName . tcsFunctions)

  case maybeType of
    Just (TypeScheme {tsType, tsGenerics}) ->
      generalise tsGenerics tsType
    Nothing -> do
      allFunctions <- gets (HM.keysSet . tcsFunctions)
      throwError (FunctionNotFound ann fnName allFunctions)

-- | look up a saved identifier "in the environment"
lookupVar :: ann -> Identifier -> TypecheckM ann (Type ann)
lookupVar ann identifier = do
  -- is it a var?
  maybeVarType <- asks (HM.lookup identifier . tceVars)
  case maybeVarType of
    Just found -> pure found
    Nothing -> do
      -- if not, is a global maybe?
      maybeGlobalType <- gets (HM.lookup identifier . tcsGlobals)
      case maybeGlobalType of
        Just (TypecheckGlobal ty _) -> pure ty
        Nothing -> do
          allVarIdentifiers <- asks (HM.keysSet . tceVars)
          allGlobalIdentifiers <- gets (HM.keysSet . tcsGlobals)
          throwError (VarNotFound ann identifier (allVarIdentifiers <> allGlobalIdentifiers))

-- | look up a saved identifier "in the environment"
lookupGlobal :: ann -> Identifier -> TypecheckM ann (TypecheckGlobal ann)
lookupGlobal ann identifier = do
  -- if not, is a global maybe?
  maybeGlobalType <- gets (HM.lookup identifier . tcsGlobals)
  case maybeGlobalType of
    Just tg -> pure tg
    Nothing -> do
      allGlobalIdentifiers <- gets (HM.keysSet . tcsGlobals)
      throwError (VarNotFound ann identifier allGlobalIdentifiers)

identifiersFromPattern :: Pattern ann -> Type ann -> TypecheckM ann [(Identifier, Type ann)]
identifiersFromPattern (PLiteral {}) _ = pure mempty
identifiersFromPattern (PVar _ identifier) ty =
  pure [(identifier, ty)]
identifiersFromPattern (PBox _ pat) (TContainer _ tys)
  | length tys == 1 =
      identifiersFromPattern pat (NE.head tys)
identifiersFromPattern (PWildcard _) _ = pure mempty
identifiersFromPattern pat@(PTuple _ p ps) ty@(TContainer _ tyItems) = do
  when
    (length (NE.tail tyItems) /= length ps)
    (throwError $ PatternMismatch ty pat)
  allIdents <- zipWithM identifiersFromPattern (p : NE.toList ps) (NE.toList tyItems)
  pure $ mconcat allIdents
identifiersFromPattern (PConstructor ann constructor ps) (TConstructor _ _ tyArgs) = do
  (_dataTypeName, dataTypeVars, dataTypeArgs) <-
    lookupConstructor ann constructor

  filtered <- matchConstructorTypesToArgs constructor dataTypeVars tyArgs dataTypeArgs

  allIdents <- zipWithM identifiersFromPattern ps filtered
  pure $ mconcat allIdents
identifiersFromPattern pat ty =
  throwError $ PatternMismatch ty pat

-- | add identifiers to the environment
withVar :: Pattern ann -> Type ann -> TypecheckM ann a -> TypecheckM ann a
withVar pat ty action = do
  idents <- HM.fromList <$> identifiersFromPattern pat ty
  local
    ( \tce ->
        tce
          { tceVars = idents <> tceVars tce
          }
    )
    action

-- | temporarily add function arguments and generics into the Reader env
withFunctionEnv ::
  [FunctionArg ann] ->
  S.Set TypeVar ->
  TypecheckM ann a ->
  TypecheckM ann a
withFunctionEnv args generics =
  let identifiers =
        fmap
          (\FunctionArg {faName = ArgumentName arg, faType} -> (Identifier arg, faType))
          args
   in local
        ( \tce ->
            tce
              { tceVars = tceVars tce <> HM.fromList identifiers,
                tceGenerics = generics
              }
        )

-- if we run `fn thing<a,b>(one:a, two: b)` as `thing((1:Int32), (2: Int64))`
-- then we know `a == Int32` and `b == Int64`.
calculateMonomorphisedTypes ::
  [TypeVar] ->
  [Type ann] ->
  [Type ann] ->
  M.Map TypeVar (Type ann) ->
  TypecheckM ann [(TypeVar, Type ann)]
calculateMonomorphisedTypes typeVars fnArgTys argTys fallbacks = do
  (fresh, freshArgTys) <-
    generaliseMany (S.fromList typeVars) fnArgTys
  traverse_ (uncurry unify) (zip argTys freshArgTys)
  unified <- gets tcsUnified
  let fixedMap = flipMap fresh
      mapped =
        foldMap
          ( \(k, a) -> case HM.lookup k fixedMap of
              Just tv -> M.singleton tv a
              Nothing -> mempty
          )
          (HM.toList unified)
      fromTv tv =
        case M.lookup tv mapped of
          Just a -> Just (tv, a)
          Nothing -> case M.lookup tv fallbacks of
            Just a -> Just (tv, a)
            Nothing -> Nothing
  pure $ mapMaybe fromTv typeVars

flipMap :: (Hashable v) => HM.HashMap k v -> HM.HashMap v k
flipMap = HM.fromList . fmap (\(k, v) -> (v, k)) . HM.toList

lookupConstructor ::
  ann ->
  Constructor ->
  TypecheckM ann (DataName, [TypeVar], [Type ann])
lookupConstructor ann constructor = do
  result <- asks (M.lookup constructor . tceDataTypes)
  case result of
    (Just (TCDataType dataType vars args)) ->
      pure (dataType, vars, args)
    Nothing ->
      throwError $ ConstructorNotFound ann constructor

-- given the arguments to a constructor, match them to the data types's vars
-- if we cannot find one (ie, because user has typed `Nothing` so we don't know
-- the `a` in `Maybe<a>`, explode, expecting a type annotation
matchConstructorTypesToArgs :: Constructor -> [TypeVar] -> [Type ann] -> [Type ann] -> TypecheckM ann [Type ann]
matchConstructorTypesToArgs constructor dataTypeVars tyArgs dataTypeArgs =
  let pairs = M.fromList (zip dataTypeVars tyArgs)
   in traverse
        ( \case
            TVar ann var -> case M.lookup var pairs of
              Just ty -> pure ty
              Nothing -> throwError (UnknownGenericInConstructor ann constructor var)
            otherTy -> pure otherTy
        )
        dataTypeArgs
