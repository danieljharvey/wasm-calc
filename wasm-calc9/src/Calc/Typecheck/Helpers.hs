{-# LANGUAGE DerivingStrategies #-}
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
  )
where

import Calc.Typecheck.Error
import Calc.Typecheck.Generalise
import Calc.Typecheck.Types
import Calc.Types.Function
import Calc.Types.Global
import Calc.Types.Identifier
import Calc.Types.Pattern
import Calc.Types.Type
import Calc.Types.TypeVar
import Control.Monad (when, zipWithM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
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
