{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Linearity.Validate
  ( validateFunction,
    validateModule,
    getFunctionUses,
  )
where

import Calc.ExprUtils
import Calc.Linearity.Error
import Calc.Linearity.Types
import Calc.TypeUtils
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Module
import Calc.Types.Pattern
import Calc.Types.Type
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.Map as M
import GHC.Natural

validateModule :: Module (Type ann) -> Either (LinearityError ann) ()
validateModule (Module {mdFunctions, mdExpr}) =
  do
    traverse_ validateFunction mdFunctions
    validate (getExprUses mdExpr)

getExprUses :: Expr (Type ann) -> LinearState ann
getExprUses expr =
  execState
    (decorateWithUses expr)
    ( LinearState
        { lsUses = mempty,
          lsVars = mempty
        }
    )

validateFunction :: Function (Type ann) -> Either (LinearityError ann) ()
validateFunction =
  validate . getFunctionUses

validate :: LinearState ann -> Either (LinearityError ann) ()
validate (LinearState {lsVars, lsUses}) =
  let validateFunctionItem (ident, (linearity, ann)) =
        case linearity of
          LTPrimitive ->
            if countCompleteUses lsUses ident == 0
              then Left (NotUsed ann ident)
              else Right ()
          LTBoxed ->
            case countCompleteUses lsUses ident of
              0 ->
                if countPartialUses lsUses ident > 0
                  then Right ()
                  else Left (NotUsed ann ident)
              1 -> Right ()
              _ -> Left (UsedMultipleTimes ident)
   in traverse_ validateFunctionItem (M.toList lsVars)

-- | count uses of a given identifier
countCompleteUses :: [(Identifier, Linearity)] -> Identifier -> Natural
countCompleteUses uses ident =
  foldr
    ( \(thisIdent, linearity) total -> case linearity of
        Whole ->
          if thisIdent == ident then total + 1 else total
        _ -> total
    )
    0
    uses

-- | count uses of a given identifier
countPartialUses :: [(Identifier, Linearity)] -> Identifier -> Natural
countPartialUses uses ident =
  foldr
    ( \(thisIdent, linearity) total -> case linearity of
        Whole -> total
        Slice _ ->
          if thisIdent == ident then total + 1 else total
    )
    0
    uses

getFunctionUses :: Function (Type ann) -> LinearState ann
getFunctionUses (Function {fnBody, fnArgs}) =
  execState
    (decorateWithUses fnBody)
    ( LinearState
        { lsVars = initialVars,
          lsUses = mempty
        }
    )
  where
    initialVars =
      foldMap
        ( \(FunctionArg {faAnn, faName = ArgumentName arg, faType}) ->
            M.singleton (Identifier arg) $ case faType of
              TPrim {} -> (LTPrimitive, getOuterTypeAnnotation faAnn)
              _ -> (LTBoxed, getOuterTypeAnnotation faAnn)
        )
        fnArgs

recordUse :: (MonadState (LinearState ann) m) => Identifier -> m ()
recordUse ident =
  modify (\ls -> ls {lsUses = (ident, Whole) : lsUses ls})

recordContainerAccessUse ::
  (MonadState (LinearState ann) m) =>
  Natural ->
  Identifier ->
  m ()
recordContainerAccessUse index ident =
  modify
    ( \ls ->
        ls
          { lsUses = (ident, Slice index) : lsUses ls
          }
    )

addLetBinding ::
  (MonadState (LinearState ann) m) =>
  Pattern (Type ann) ->
  Type ann ->
  m ()
addLetBinding (PVar _ ident) ty =
  let initialLinearity = case ty of
        TPrim {} -> LTPrimitive
        _ -> LTBoxed
   in modify
        ( \ls ->
            ls
              { lsVars =
                  M.insert ident (initialLinearity, getOuterTypeAnnotation ty) (lsVars ls)
              }
        )
addLetBinding _ _ = error "addLetBinding with other pattern"

decorateWithUses ::
  (MonadState (LinearState ann) m) =>
  Expr (Type ann) ->
  m (Expr (Type ann))
decorateWithUses (EVar ann ident) = do
  recordUse ident
  pure (EVar ann ident)
decorateWithUses (EContainerAccess ann (EVar ann' ident) index) =
  do
    recordContainerAccessUse index ident
    pure (EContainerAccess ann (EVar ann' ident) index)
decorateWithUses (ELet ann ident expr rest) = do
  addLetBinding ident (getOuterAnnotation expr)
  ELet ann ident <$> decorateWithUses expr <*> decorateWithUses rest
decorateWithUses other =
  bindExpr decorateWithUses other
