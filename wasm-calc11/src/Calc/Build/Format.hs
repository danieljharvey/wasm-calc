{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Calc.Build.Format
  (
    formatAndSave,
    format,
  )
where

import Calc.Types.Module
import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Error.Diagnose.Compat.Megaparsec
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

instance HasHints Void msg where
  hints _ = mempty

-- format the file, and if it's changed, save it
format :: [ModuleItem ann] -> T.Text
format parsedModuleItems = do
  let prettyMod = PP.cat (PP.punctuate PP.line (PP.pretty <$> parsedModuleItems))
   in renderWithWidth 60 prettyMod

-- format the file, and if it's changed, save it
formatAndSave :: (MonadIO m) => FilePath -> T.Text -> [ModuleItem ann] -> m ()
formatAndSave filePath originalInput parsedModuleItems = do
  let printed = format parsedModuleItems
  when (printed /= originalInput) $
    liftIO $
      T.writeFile filePath printed

renderWithWidth :: Int -> PP.Doc ann -> T.Text
renderWithWidth w doc = PP.renderStrict (PP.layoutPretty layoutOptions (PP.unAnnotate doc))
  where
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine w 1}
