{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Linearity.Error
  ( linearityErrorDiagnostic,
    LinearityError (..),
  )
where

import Calc.SourceSpan
import Calc.Types.Annotation
import Calc.Types.Identifier
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

data LinearityError ann
  = NotUsed ann Identifier
  | UsedMultipleTimes (NE.NonEmpty ann) Identifier
  deriving stock (Eq, Ord, Show)

prettyPrint :: PP.Doc doc -> T.Text
prettyPrint = renderWithWidth 60

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

linearityErrorDiagnostic ::
  T.Text ->
  LinearityError Annotation ->
  Diag.Diagnostic T.Text
linearityErrorDiagnostic input e =
  let filename = "<repl>"
      diag = Diag.addFile mempty filename (T.unpack input)
      report = case e of
        (NotUsed ann ident) ->
          Diag.Err
            Nothing
            ( prettyPrint $ "Identifier " <> PP.pretty ident <> " not used."
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This
                          ( prettyPrint "This has not been used"
                          )
                      )
                ]
            )
            []
        (UsedMultipleTimes anns ident) ->
          Diag.Err
            Nothing
            ( prettyPrint $ "Identifier " <> PP.pretty ident <> " used multiple times."
            )
            ( mapMaybe
                ( \ann ->
                    (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.Where
                            ( prettyPrint "Used here"
                            )
                        )
                )
                (NE.toList anns)
            )
            []
   in Diag.addReport diag report

renderWithWidth :: Int -> PP.Doc ann -> T.Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}
