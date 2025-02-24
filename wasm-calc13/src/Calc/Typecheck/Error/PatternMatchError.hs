{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Typecheck.Error.PatternMatchError
  ( PatternMatchError (..),
    patternMatchErrorDiagnostic,
  )
where

import Calc.SourceSpan
import Calc.Types.Annotation
import Calc.Types.Pattern
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

data PatternMatchError ann
  = -- | Cases not covered in pattern matches
    -- | ann, [missing patterns]
    MissingPatterns ann [Pattern ()]
  | -- | Unnecessary cases covered by previous matches
    RedundantPatterns ann [Pattern ann]
  deriving stock (Eq, Ord, Show, Foldable)

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

patternMatchErrorDiagnostic ::
  T.Text ->
  PatternMatchError Annotation ->
  Diag.Diagnostic T.Text
patternMatchErrorDiagnostic input e =
  let filename = "<repl>"
      diag = Diag.addFile mempty filename (T.unpack input)
      report = case e of
        (RedundantPatterns ann redundant) ->
          Diag.Err
            Nothing
            ( prettyPrint "Pattern match has unreachable patterns, you should remove them"
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This
                          ( prettyPrint $
                              PP.cat (PP.punctuate PP.line (PP.pretty <$> redundant))
                          )
                      )
                ]
            )
            []
        (MissingPatterns ann missing) ->
          Diag.Err
            Nothing
            ( prettyPrint "Pattern match is missing some patterns"
            )
            ( catMaybes
                [ (,)
                    <$> positionFromAnnotation
                      filename
                      input
                      ann
                    <*> pure
                      ( Diag.This
                          ( prettyPrint $
                              PP.cat (PP.punctuate PP.line (PP.pretty <$> missing))
                          )
                      )
                ]
            )
            []
   in Diag.addReport diag report

renderWithWidth :: Int -> PP.Doc ann -> T.Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}
