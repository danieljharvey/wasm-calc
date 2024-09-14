{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Calc.PrettyPrint
  ( prettyPrint,
  )
where

import Calc.Build.Format
import Calc.Build.Print
import Calc.Build.Steps
import Control.Monad.IO.Class
import Data.Functor (($>))
import qualified Data.Text as T
import System.Exit

prettyPrint :: FilePath -> IO ()
prettyPrint filePath =
  liftIO $ doPrettyPrint filePath >>= exitWith

doPrettyPrint :: (MonadIO m) => FilePath -> m ExitCode
doPrettyPrint filePath = do
  input <- liftIO (readFile filePath)
  case parseModuleStep (T.pack input) of
    Left buildError -> printBuildError buildError $> ExitFailure 1
    Right parsedModule -> do
      formatAndSave filePath (T.pack input) parsedModule
      pure ExitSuccess
