{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Calc.PrettyPrint
  ( prettyPrint,
    format,
  )
where

import Calc.Parser
import Calc.Parser.Types
import Calc.Types.Module
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import System.Exit

instance HasHints Void msg where
  hints _ = mempty

prettyPrint :: FilePath -> IO ()
prettyPrint filePath =
  liftIO $ doPrettyPrint filePath >>= exitWith

doPrettyPrint :: (MonadIO m) => FilePath -> m ExitCode
doPrettyPrint filePath = do
  input <- liftIO (readFile filePath)
  case parseModule (T.pack input) of
    Left bundle ->
      do
        printDiagnostic (fromErrorBundle bundle input)
        >> pure (ExitFailure 1)
    Right parsedModule -> do
      format filePath (T.pack input) parsedModule
      pure ExitSuccess

-- format the file, and if it's changed, save it
format :: (MonadIO m) => FilePath -> Text -> Module ann -> m ()
format filePath originalInput parsedModule = do
  let printed = renderWithWidth 60 (PP.pretty parsedModule)
  when (printed /= originalInput) $
    liftIO $
      T.writeFile filePath printed

renderWithWidth :: Int -> PP.Doc ann -> T.Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}

printDiagnostic :: (MonadIO m) => Diag.Diagnostic Text -> m ()
printDiagnostic =
  Diag.printDiagnostic
    Diag.stderr
    Diag.WithUnicode
    (Diag.TabSize 4)
    Diag.defaultStyle

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
