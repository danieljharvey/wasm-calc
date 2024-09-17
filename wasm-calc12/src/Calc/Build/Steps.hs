{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Calc.Build.Steps
  ( BuildError (..),
    parseModuleStep,
    resolveModuleStep,
    typecheckModuleStep,
    linearityCheckStep,
    abilityCheckStep,
    fromExprStep,
    testsAllPass,
    displayResults,
  )
where

import Calc.Types.WithPath
import qualified Data.Map.Strict as M
import Calc.Ability.Check
import Calc.Dependencies
import qualified Calc.Linearity as Linearity
import Calc.Module (resolveModules)
import Calc.Parser
import Calc.Parser.Types
import Calc.Typecheck
import Calc.Types.Ability
import Calc.Types.Annotation
import Calc.Types.Module
import Calc.Types.Type
import Calc.Wasm.FromExpr.Module
import Calc.Wasm.ToWasm.Types (WasmModule)
import Control.Monad.Except
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec

data BuildError
  = BuildDiagnostic (Diag.Diagnostic T.Text)
  | BuildMessage Text

instance HasHints Void msg where
  hints _ = mempty

parseModuleStep :: T.Text -> Either BuildError [ModuleItem Annotation]
parseModuleStep input =
  case parseModule input of
    Right a -> pure a
    Left bundle -> throwError $ BuildDiagnostic (fromErrorBundle bundle (T.unpack input))

resolveModuleStep :: [ModuleItem Annotation] -> Either BuildError (M.Map ModulePath (Module Annotation))
resolveModuleStep parsedModuleItems =
  case resolveModules parsedModuleItems of
    Right a -> pure a
    Left err -> throwError $ BuildMessage (T.pack $ show err)

typecheckModuleStep :: T.Text -> Module Annotation -> Either BuildError (Module (Type Annotation))
typecheckModuleStep input parsedModule =
  case elaborateModule parsedModule of
    Right a -> pure a
    Left typeErr ->
      throwError $ BuildDiagnostic (typeErrorDiagnostic input typeErr)

linearityCheckStep :: T.Text -> Module (Type Annotation) -> Either BuildError ()
linearityCheckStep input typedModule =
  case Linearity.validateModule typedModule of
    Right a -> pure a
    Left linearityError ->
      throwError $ BuildDiagnostic (Linearity.linearityErrorDiagnostic input linearityError)

abilityCheckStep :: T.Text -> Module Annotation ->
      Either BuildError (ModuleAnnotations (S.Set (Ability Annotation)))
abilityCheckStep input parsedModule =
  case abilityCheckModule parsedModule of
    Right a -> pure a
    Left abilityError ->
      throwError $ BuildDiagnostic (abilityErrorDiagnostic input abilityError)

fromExprStep :: Module (Type Annotation) -> Either BuildError WasmModule
fromExprStep typedModule =
  case fromModule (treeShakeModule typedModule) of
    Right wasmMod -> pure wasmMod
    Left fromWasmError -> do
      throwError (BuildMessage $ T.pack $ show fromWasmError)

testsAllPass :: [(a, Bool)] -> Bool
testsAllPass = getAll . foldMap (All . snd)

displayResults :: [(T.Text, Bool)] -> [T.Text]
displayResults =
  fmap (T.pack . printResult)
  where
    printResult (name, True) =
      "✅ " <> show name
    printResult (name, False) =
      "❌ " <> show name

-- | turn Megaparsec error + input into a Diagnostic
fromErrorBundle :: ParseErrorType -> String -> Diag.Diagnostic Text
fromErrorBundle bundle input =
  let diag =
        errorDiagnosticFromBundle
          Nothing
          "Parse error on input"
          Nothing
          bundle
   in Diag.addFile diag replFilename input
