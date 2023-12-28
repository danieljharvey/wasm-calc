{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Calc.Typecheck.Helpers
  ( runTypecheckM,
    unifyVariableWithType,
    lookupVar,
    withVar,
    lookupFunction,
    withFunctionEnv,
    storeFunction,
  )
where
import           Calc.Typecheck.Error
import           Calc.Typecheck.Generalise
import           Calc.Typecheck.Types
import           Calc.Types.Function
import           Calc.Types.Identifier
import           Calc.Types.Type
import           Calc.Types.TypeVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor            (first)
import qualified Data.HashMap.Strict       as HM
import qualified Data.Set                  as S
import           GHC.Natural

runTypecheckM ::
  TypecheckEnv ann ->
  TypecheckM ann a ->
  Either (TypeError ann) a
runTypecheckM env action =
  evalStateT
    (runReaderT (getTypecheckM action) env)
    ( TypecheckState
        { tcsFunctions = mempty,
          tcsUnique = 0,
          tcsUnified = mempty
        }
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

-- | look up a saved identifier "in the environment"
lookupFunction ::  ann -> FunctionName -> TypecheckM ann (Type ann)
lookupFunction ann fnName = do
  maybeType <- gets (HM.lookup fnName .  tcsFunctions)

  case maybeType of
    Just (TypeScheme {tsType, tsGenerics}) ->
      generalise tsGenerics tsType
    Nothing -> do
      allFunctions <- gets (HM.keysSet . tcsFunctions)
      throwError (FunctionNotFound ann fnName allFunctions)

-- | look up a saved identifier "in the environment"
lookupVar :: ann -> Identifier -> TypecheckM ann (Type ann)
lookupVar ann identifier = do
  maybeType <- asks (HM.lookup identifier . tceVars)
  case maybeType of
    Just found -> pure found
    Nothing -> do
      allIdentifiers <- asks (HM.keysSet . tceVars)
      throwError (VarNotFound ann identifier allIdentifiers)

-- | add an identifier to the environment
withVar :: Identifier -> Type ann -> TypecheckM ann a -> TypecheckM ann a
withVar identifier ty =
  local
    ( \tce ->
        tce
          { tceVars =
              HM.insert identifier ty (tceVars tce)
          }
    )

-- | temporarily add function arguments and generics into the Reader env
withFunctionEnv ::
  [(ArgumentName, Type ann)] ->
  S.Set TypeVar ->
  TypecheckM ann a ->
  TypecheckM ann a
withFunctionEnv args generics =
  let identifiers = fmap (first (\(ArgumentName arg) -> Identifier arg)) args
   in local
        ( \tce ->
            tce
              { tceVars = tceVars tce <> HM.fromList identifiers,
                tceGenerics = generics
              }
        )

-- | given a unification variable, either save it and return the type
-- or explode because we've already unified it with something else
unifyVariableWithType ::
  Natural ->
  Type ann ->
  TypecheckM ann (Type ann)
unifyVariableWithType nat ty =
  do
    existing <- gets (HM.lookup nat . tcsUnified)
    case existing of
      Nothing -> do
        -- this is the first match, store it and return the passed-in type
        modify
          ( \tcs ->
              tcs
                { tcsUnified =
                    HM.insert nat ty (tcsUnified tcs)
                }
          )
        pure ty
      Just _existingTy -> do
        -- another type matches this unification variable
        -- for now, explode
        -- in future we'll compare them
        error "unifyVariableWithType"
