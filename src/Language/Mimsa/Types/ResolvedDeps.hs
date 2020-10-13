{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.ResolvedDeps where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.StoreExpression

newtype ResolvedDeps a
  = ResolvedDeps
      { getResolvedDeps ::
          Map Name
            (ExprHash, StoreExpression a)
      }

hasNoDeps :: ResolvedDeps a -> Bool
hasNoDeps (ResolvedDeps m) = M.size m == 0

instance Printer (ResolvedDeps a) where
  prettyDoc (ResolvedDeps deps) =
    encloseSep
      lbrace
      rbrace
      comma
      (prettyDoc <$> M.keys deps)