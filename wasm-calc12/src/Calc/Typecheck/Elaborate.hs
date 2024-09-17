{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calc.Typecheck.Elaborate
  ( elaborateFunction,
    elaborateModule,elaborateModules
  )
where

import Calc.Types.WithPath
import qualified Data.Map as M
import Calc.ExprUtils
import Calc.Typecheck.Error
import Calc.Typecheck.Helpers
import Calc.Typecheck.Infer
import Calc.Typecheck.Substitute
import Calc.Typecheck.Types
import Calc.Types.Data
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Global
import Calc.Types.Import
import Calc.Types.Memory
import Calc.Types.Module
import Calc.Types.Test
import Calc.Types.Type
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.Set as S

elaborateModules :: M.Map ModulePath (Module ann) -> Either (TypeError ann) (M.Map ModulePath (Module (Type ann)))
elaborateModules = 
  traverse elaborateModule 



elaborateModule ::
  forall ann.
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule
  ( Module
      { mdTests,
        mdImports,
        mdGlobals,
        mdMemory,
        mdFunctions,
        mdDataTypes
      }
    ) = do
    let typecheckEnv =
          TypecheckEnv
            { tceVars = mempty,
              tceGenerics = mempty,
              tceMemoryLimit = case mdMemory of
                Nothing -> 0
                Just (LocalMemory {lmLimit}) -> lmLimit
                Just (ImportedMemory {imLimit}) -> imLimit,
              tceDataTypes = arrangeDataTypes mdDataTypes
            }

    runTypecheckM typecheckEnv $ do
      -- first store all the function's unchecked types
      -- TODO: include ALL available functions for this module
      traverse_
        ( \fn ->
            storeFunction
              (fnFunctionName fn)
              mempty
              (S.fromList $ fnGenerics fn)
              (TFunction (fnAnn fn) (faType <$> fnArgs fn) (fnReturnType fn))
        )
        mdFunctions

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
              storeFunction (impImportName elabImport) mempty mempty (impAnn elabImport)
              pure elabImport
          )
          mdImports

      -- actually typecheck functions
      -- we don't need to store these as we go anymore
      functions <-
        traverse
          ( \fn -> do
              elabFn <- elaborateFunction fn
              storeFunction
                (fnFunctionName elabFn)
                mempty
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
            mdTests = tests,
            mdDataTypes = elaborateDataType <$> mdDataTypes
          }

elaborateDataType :: Data ann -> Data (Type ann)
elaborateDataType (Data dtName vars cons) =
  Data dtName vars ((fmap . fmap) (\ty -> ty $> ty) cons)

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
elaborateFunction
  ( Function
      { fnPublic,
        fnAnn,
        fnArgs,
        fnAbilityConstraints,
        fnGenerics,
        fnReturnType,
        fnFunctionName,
        fnBody
      }
    ) = do
    -- store current function so we can recursively call ourselves
    -- TODO: don't think this is required anymore as we statically add all
    -- function type signatures before starting elaboration 
    storeFunction
      fnFunctionName
      mempty
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
            fnReturnType = fnReturnType $> fnReturnType,
            fnAbilityConstraints = fnAbilityConstraints
          }
      )
