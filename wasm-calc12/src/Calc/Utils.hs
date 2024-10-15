module Calc.Utils (prettyShow,
    ltrace, ltraceM, neZipWith, neZipWithM, neUnzip, indentMulti, newlines) where

-- useful junk goes here

import Control.Monad (zipWithM)
import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as TL
import qualified Debug.Trace as Debug
import qualified Prettyprinter as PP
import qualified Text.Pretty.Simple as PS

neZipWithM ::
  (Applicative m) =>
  (a -> b -> m c) ->
  NE.NonEmpty a ->
  NE.NonEmpty b ->
  m (NE.NonEmpty c)
neZipWithM f as bs =
  NE.fromList <$> zipWithM f (NE.toList as) (NE.toList bs)

neZipWith ::
  (a -> b -> c) ->
  NE.NonEmpty a ->
  NE.NonEmpty b ->
  NE.NonEmpty c
neZipWith f as bs =
  NE.fromList $ zipWith f (NE.toList as) (NE.toList bs)

neUnzip :: NE.NonEmpty (a, b) -> (NE.NonEmpty a, NE.NonEmpty b)
neUnzip = bimap NE.fromList NE.fromList . unzip . NE.toList

ltrace :: (Show a) => String -> a -> a
ltrace lbl x = Debug.trace (lbl <> ": " <> TL.unpack (PS.pShow x)) x

ltraceM :: (Show a, Monad m) => String -> a -> m ()
ltraceM lbl x = Debug.traceM (lbl <> ": " <> TL.unpack (PS.pShow x))

prettyShow :: (Show a) => a -> String
prettyShow = TL.unpack . PS.pShow

-- when on multilines, indent by `i`, if not then nothing
indentMulti :: Integer -> PP.Doc style -> PP.Doc style
indentMulti i doc =
  PP.flatAlt (PP.indent (fromIntegral i) doc) doc

newlines :: PP.Doc style -> PP.Doc style
newlines a = PP.line' <> a <> PP.line'
