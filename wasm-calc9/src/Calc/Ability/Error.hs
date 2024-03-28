{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Calc.Ability.Error (AbilityError (..), abilityErrorDiagnostic) where

import           Calc.SourceSpan
import           Calc.Types.Ability
import           Calc.Types.Annotation
import           Calc.Types.Function
import           Calc.Types.Identifier
import           Data.Maybe                (catMaybes)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Error.Diagnose            as Diag
import qualified Prettyprinter             as PP
import qualified Prettyprinter.Render.Text as PP

data AbilityError ann =
  FunctionViolatesConstraint
    { aeConstraint :: AbilityConstraint,
        aeAbility :: Ability ann, aeFunctionName :: FunctionName }
  |  TestViolatesConstraint
  { aeAbility  :: Ability ann,
    aeTestName :: Identifier
  }
  deriving stock (Eq, Ord, Show, Functor)

positionFromAnnotation ::
  String ->
  T.Text ->
  Annotation ->
  Maybe Diag.Position
positionFromAnnotation path input ann =
  let toPos ss =
        Diag.Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
   in toPos <$> sourceSpan input ann

prettyPrint :: PP.Doc doc -> T.Text
prettyPrint = renderWithWidth 60
  where
    renderWithWidth w doc =
      PP.renderStrict (PP.layoutPretty (layoutOptions w) (PP.unAnnotate doc))

    layoutOptions w =
      PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}

getAbilityAnnotation :: Ability ann -> ann
getAbilityAnnotation (CallImportedFunction ann _) = ann
getAbilityAnnotation (AllocateMemory ann)         = ann
getAbilityAnnotation (MutateGlobal ann _)         = ann

abilityErrorDiagnostic ::
  T.Text ->
  AbilityError Annotation ->
  Diag.Diagnostic Text
abilityErrorDiagnostic input e =
  let filename = "<repl>"
      diag = Diag.addFile mempty filename (T.unpack input)
      report = case e of
        (TestViolatesConstraint {aeAbility, aeTestName}) ->
          Diag.Err
            Nothing
            ( prettyPrint $ "Test " <> PP.dquotes (PP.pretty aeTestName) <> " is using abilities it should not."
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getAbilityAnnotation aeAbility)
                    <*> pure
                      ( Diag.This
                          ( prettyPrint $ PP.pretty aeAbility
                          )
                      )
                ]
            )
            ( case aeAbility of
                CallImportedFunction {} -> ["Tests are run in an interpreter and so they are not allowed to use imported functions"]

                _ -> []
            )
        (FunctionViolatesConstraint {aeAbility, aeConstraint, aeFunctionName}) ->
          Diag.Err
            Nothing
            ( prettyPrint $ "Function " <> PP.dquotes (PP.pretty aeFunctionName) <> " is using abilities it should not."
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      (getAbilityAnnotation aeAbility)
                    <*> pure
                      ( Diag.This
                          ( prettyPrint $ PP.pretty aeAbility
                          )
                      )
                ]
            )
            [ Diag.Note $ prettyPrint $ "This is disallowed because the function has the the " <> PP.dquotes (PP.pretty aeConstraint) <> " constraint."]

   in Diag.addReport diag report
