{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calc.Typecheck.Elaborate
  ( elaborateFunction,
    elaborateModule,
  )
where

import Calc.ExprUtils
import Calc.Typecheck.Error
import Calc.Typecheck.Helpers
import Calc.Typecheck.Infer
import Calc.Typecheck.Substitute
import Calc.Typecheck.Types
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Import
import Calc.Types.Module
import Calc.Types.Type
import Control.Monad.State
import Data.Functor
import qualified Data.Set as S

elaborateModule ::
  forall ann.
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule (Module {mdImports, mdMemory, mdFunctions}) = do
  let typecheckEnv =
        TypecheckEnv
          { tceVars = mempty,
            tceGenerics = mempty,
            tceMemoryLimit = case mdMemory of
              Nothing -> 0
              Just (LocalMemory {lmLimit}) -> lmLimit
              Just (ImportedMemory {imLimit}) -> imLimit
          }

  runTypecheckM typecheckEnv $ do
    imports <-
      traverse
        ( \imp -> do
            elabImport <- elaborateImport imp
            storeFunction (impImportName elabImport) mempty (impAnn elabImport)
            pure elabImport
        )
        mdImports

    fns <-
      traverse
        ( \fn -> do
            elabFn <- elaborateFunction fn
            storeFunction
              (fnFunctionName elabFn)
              (S.fromList $ fnGenerics fn)
              (fnAnn elabFn)
            pure elabFn
        )
        mdFunctions

    pure $
      Module
        { mdFunctions = fns,
          mdImports = imports,
          mdMemory = elaborateMemory <$> mdMemory
        }

-- decorate a memory annotation with an arbitrary Void type
elaborateMemory :: Memory ann -> Memory (Type ann)
elaborateMemory = fmap (`TPrim` TVoid)

elaborateImport :: Import ann -> TypecheckM ann (Import (Type ann))
elaborateImport
  Import
    { impArgs,
      impExternalModule,
      impReturnType,
      impAnn,
      impExternalFunction,
      impImportName
    } = do
    let importArguments =
          ( \ImportArg {iaName, iaType, iaAnn} ->
              ImportArg
                { iaName,
                  iaType = fmap (const iaType) iaType,
                  iaAnn = fmap (const iaAnn) iaType
                }
          )
            <$> impArgs

    let importType =
          TFunction
            impAnn
            (iaType <$> impArgs)
            impReturnType

    pure $
      Import
        { impImportName,
          impExternalModule,
          impExternalFunction,
          impAnn = importType,
          impArgs = importArguments,
          impReturnType = fmap (const impReturnType) impReturnType
        }

checkAndSubstitute ::
  Type ann ->
  Expr ann ->
  TypecheckM ann (Expr (Type ann))
checkAndSubstitute ty expr = do
  exprA <- check ty expr
  unified <- gets tcsUnified
  pure $ substitute unified <$> exprA

elaborateFunction ::
  Function ann ->
  TypecheckM ann (Function (Type ann))
elaborateFunction (Function {fnPublic, fnAnn, fnArgs, fnGenerics, fnReturnType, fnFunctionName, fnBody}) = do
  exprA <-
    withFunctionEnv
      fnArgs
      (S.fromList fnGenerics)
      (checkAndSubstitute fnReturnType fnBody)
  let argsA =
        ( \FunctionArg {faName, faType, faAnn} ->
            FunctionArg
              { faName,
                faType = fmap (const faType) faType,
                faAnn = fmap (const faAnn) faType
              }
        )
          <$> fnArgs
  let tyFn =
        TFunction
          fnAnn
          (faType <$> fnArgs)
          (getOuterAnnotation exprA)
  pure
    ( Function
        { fnAnn = tyFn,
          fnGenerics,
          fnArgs = argsA,
          fnFunctionName = fnFunctionName,
          fnBody = exprA,
          fnPublic = fnPublic,
          fnReturnType = fnReturnType $> fnReturnType
        }
    )
