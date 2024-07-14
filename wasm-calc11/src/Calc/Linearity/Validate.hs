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
validateGlobal glob =
  let (expr, linearState) = getGlobalUses glob
   in validate linearState $> expr

validateFunction ::
  (Show ann) =>
  Function (Type ann) ->
  Either (LinearityError ann) (Expr (Type ann, Maybe (Drops ann)))
validateFunction fn =
  let (expr, linearState) = getFunctionUses fn
   in validate linearState $> expr

validate :: LinearState ann -> Either (LinearityError ann) ()
validate (LinearState {lsVars, lsUses}) =
  let validateFunctionItem (Internal _, _) = Right ()
      validateFunctionItem (UserDefined ident, (linearity, ann)) =
        let completeUses = filterCompleteUses lsUses ident
         in case linearity of
              LTPrimitive ->
                if null completeUses
                  then Left (NotUsed ann ident)
                  else Right ()
              LTBoxed ->
                case length completeUses of
                  0 -> Left (NotUsed ann ident)
                  1 -> Right ()
                  _more ->
                    Left (UsedMultipleTimes (getLinearityAnnotation <$> completeUses) ident)
   in traverse_ validateFunctionItem (M.toList lsVars)

-- | count uses of a given identifier
filterCompleteUses ::
  [(Identifier, Linearity ann)] ->
  Identifier ->
  [Linearity ann]
filterCompleteUses uses ident =
  foldr
    ( \(thisIdent, linearity) total -> case linearity of
        Whole _ ->
          if thisIdent == ident then linearity : total else total
    )
    []
    uses

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
          lsUses = mempty,
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
          lsUses = mempty,
          lsFresh = 0
        }
