{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Linearity.Validate
  ( validateFunction,
    validateGlobal,
    validateModule,
    getFunctionUses,
  )
where

import Calc.Linearity.Decorate
import Calc.Linearity.Error
import Calc.Linearity.Types
import Calc.TypeUtils
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Global
import Calc.Types.Identifier
import Calc.Types.Import
import Calc.Types.Module
import Calc.Types.Type
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S

validateModule :: (Show ann) => Module (Type ann) -> Either (LinearityError ann) ()
validateModule (Module {mdImports, mdFunctions, mdGlobals}) = do
  let functionNames =
        foldMap (S.singleton . fnFunctionName) mdFunctions
          <> foldMap (S.singleton . impImportName) mdImports
  traverse_ (validateFunction functionNames) mdFunctions
  traverse_ validateGlobal mdGlobals

validateGlobal ::
  (Show ann) =>
  Global (Type ann) ->
  Either (LinearityError ann) (Expr (Type ann, Maybe (Drops ann)))
validateGlobal glob = do
  (expr, linearState) <- getGlobalUses glob
  validate linearState $> expr

validateFunction ::
  (Show ann) =>
  S.Set FunctionName ->
  Function (Type ann) ->
  Either (LinearityError ann) (Expr (Type ann, Maybe (Drops ann)))
validateFunction functionNames fn = do
  (expr, linearState) <- getFunctionUses functionNames fn
  validate linearState $> expr

validate :: LinearState ann -> Either (LinearityError ann) ()
validate (LinearState {lsVars, lsUses}) =
  let validateFunctionItem (Internal _, _) = Right ()
      validateFunctionItem (UserDefined ident, (linearity, ann)) =
        let linearState = M.lookup ident (NE.head lsUses)
         in case linearity of
              LTPrimitive ->
                case linearState of
                  Nothing -> Left (NotUsed ann ident)
                  _ -> Right ()
              LTBoxed ->
                case linearState of
                  Just (Fresh _) -> Left (NotUsed ann ident)
                  Just (Used _) -> Right ()
                  Nothing -> Left (NotUsed ann ident)
   in traverse_ validateFunctionItem (M.toList lsVars)

getFunctionUses ::
  (Show ann) =>
  S.Set FunctionName ->
  Function (Type ann) ->
  Either
    (LinearityError ann)
    (Expr (Type ann, Maybe (Drops ann)), LinearState ann)
getFunctionUses functionNames (Function {fnFunctionName = FunctionName fnName, fnBody, fnArgs}) =
  fst <$> runIdentity $ runWriterT $ runExceptT $ runStateT action initialState
  where
    action = decorate fnBody

    ignoreVars =
      S.map (\(FunctionName ident) -> Identifier ident) functionNames
        <> S.singleton (Identifier fnName) -- don't count recursive calls
    initialState =
      LinearState
        { lsVars = initialVars,
          lsUses = NE.singleton mempty,
          lsFresh = 0,
          lsIgnoreVars = ignoreVars
        }

    initialVars =
      foldMap
        ( \(FunctionArg {faAnn, faName = ArgumentName arg, faType}) ->
            M.singleton (UserDefined (Identifier arg)) $ case faType of
              TPrim {} -> (LTPrimitive, getOuterTypeAnnotation faAnn)
              _ -> (LTBoxed, getOuterTypeAnnotation faAnn)
        )
        fnArgs

getGlobalUses ::
  (Show ann) =>
  Global (Type ann) ->
  Either
    (LinearityError ann)
    (Expr (Type ann, Maybe (Drops ann)), LinearState ann)
getGlobalUses (Global {glbExpr}) =
  fst <$> runIdentity $ runWriterT (runExceptT (runStateT action initialState))
  where
    action = decorate glbExpr

    initialState =
      LinearState
        { lsVars = mempty,
          lsUses = NE.singleton mempty,
          lsFresh = 0,
          lsIgnoreVars = mempty
        }
