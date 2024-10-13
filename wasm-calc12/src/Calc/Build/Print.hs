{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Calc.Build.Print (printBuildError) where

import Calc.Build.Steps
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import qualified Error.Diagnose as Diag

printBuildError :: (MonadIO m) => BuildError -> m ()
printBuildError (BuildDiagnostic diag) =
  Diag.printDiagnostic
    Diag.stderr
    Diag.WithUnicode
    (Diag.TabSize 4)
    Diag.defaultStyle
    diag
printBuildError (BuildMessage msg) =
  liftIO (T.hPutStrLn Diag.stderr msg)
