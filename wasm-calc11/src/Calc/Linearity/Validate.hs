{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Linearity.Validate
  ( validateFunction,
    validateGlobal,
    validateModule,
    getFunctionUses,
  )
where

import qualified Data.List.NonEmpty as NE
import Calc.Linearity.Decorate
import Calc.Linearity.Error
import Calc.Linearity.Types
import Calc.TypeUtils
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Global
import Calc.Types.Identifier
import Calc.Types.Module
import Calc.Types.Type
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Map as M

getLinearityAnnotation :: Linearity ann -> ann
getLinearityAnnotation (Whole ann) = ann

validateModule :: (Show ann) => Module (Type ann) -> Either (LinearityError ann) ()
validateModule (Module {mdFunctions, mdGlobals}) = do
  traverse_ validateFunction mdFunctions
  traverse_ validateGlobal mdGlobals

validateGlobal ::
  (Show ann) =>
  Global (Type ann) ->
  Either (LinearityError ann) (Expr (Type ann, Maybe (Drops ann)))
validateGlobal glob = do
  let  (expr, linearState) = getGlobalUses glob
  validate linearState $> expr

validateFunction ::
  (Show ann) =>
  Function (Type ann) ->
  Either (LinearityError ann) (Expr (Type ann, Maybe (Drops ann)))
validateFunction fn = do
  let (expr, linearState) = getFunctionUses fn
  validate linearState $> expr

validate :: LinearState ann -> Either (LinearityError ann) ()
validate (LinearState {lsVars, lsUses}) =
  let validateFunctionItem (Internal _, _) = Right ()
      validateFunctionItem (UserDefined ident, (linearity, ann)) =
        let completeUses = maybe mempty NE.toList (M.lookup ident (NE.head lsUses))
         in case linearity of
              LTPrimitive ->
                if null completeUses
                  then Left (NotUsed ann ident)
                  else Right ()
              LTBoxed ->
                case NE.nonEmpty completeUses of
                  Nothing -> Left (NotUsed ann ident)
                  Just neUses ->
                    if length neUses == 1
                       then Right ()
                       else
                          Left (UsedMultipleTimes (getLinearityAnnotation <$> neUses) ident)
   in traverse_ validateFunctionItem (M.toList lsVars)

getFunctionUses ::
  (Show ann) =>
  Function (Type ann) ->
  (Expr (Type ann, Maybe (Drops ann)), LinearState ann)
getFunctionUses (Function {fnBody, fnArgs}) =
  fst $ runIdentity $ runWriterT $ runStateT action initialState
  where
    action = decorate fnBody

    initialState =
      LinearState
        { lsVars = initialVars,
          lsUses = NE.singleton mempty,
          lsFresh = 0
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
    (Expr (Type ann, Maybe (Drops ann)), LinearState ann)
getGlobalUses (Global {glbExpr}) =
  fst $ runIdentity $ runWriterT $ runStateT action initialState
  where
    action = decorate glbExpr

    initialState =
      LinearState
        { lsVars = mempty,
          lsUses = NE.singleton mempty,
          lsFresh = 0
        }
