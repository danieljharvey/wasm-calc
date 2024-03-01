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
import Calc.Types.Global
import Calc.Types.Import
import Calc.Types.Memory
import Calc.Types.Module
import Calc.Types.Test
import Calc.Types.Type
import Control.Monad.State
import Data.Functor
import qualified Data.Set as S

elaborateModule ::
  forall ann.
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule (Module {mdTests, mdImports, mdGlobals, mdMemory, mdFunctions}) = do
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
    globals <-
      traverse
        ( \global -> do
            elabGlobal <- elaborateGlobal global
            storeGlobal (glbIdentifier elabGlobal) (glbMutability elabGlobal) (glbAnn elabGlobal)
            pure elabGlobal
        )
        mdGlobals

    imports <-
      traverse
        ( \imp -> do
            elabImport <- elaborateImport imp
            storeFunction (impImportName elabImport) mempty (impAnn elabImport)
            pure elabImport
        )
        mdImports

    functions <-
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

    tests <- traverse elaborateTest mdTests

    pure $
      Module
        { mdFunctions = functions,
          mdImports = imports,
          mdMemory = elaborateMemory <$> mdMemory,
          mdGlobals = globals,
          mdTests = tests
        }

-- check a test expression has type `Bool`
-- later we'll also check it does not use any imports
elaborateTest :: Test ann -> TypecheckM ann (Test (Type ann))
elaborateTest (Test {tesAnn, tesName, tesExpr}) = do
  elabExpr <- check (TPrim tesAnn TBool) tesExpr

  pure $
    Test
      { tesAnn = getOuterAnnotation elabExpr,
        tesName,
        tesExpr = elabExpr
      }

-- decorate a memory annotation with an arbitrary Void type
elaborateMemory :: Memory ann -> Memory (Type ann)
elaborateMemory = fmap (`TPrim` TVoid)

elaborateGlobal :: Global ann -> TypecheckM ann (Global (Type ann))
elaborateGlobal (Global {glbMutability, glbIdentifier, glbExpr}) = do
  elabExpr <- infer glbExpr

  pure $
    Global
      { glbAnn = getOuterAnnotation elabExpr,
        glbMutability,
        glbIdentifier,
        glbExpr = elabExpr
      }

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
  -- store current function so we can recursively call ourselves
  storeFunction
    fnFunctionName
    (S.fromList fnGenerics)
    (TFunction fnAnn (faType <$> fnArgs) fnReturnType)

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
