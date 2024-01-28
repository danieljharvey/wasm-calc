{-# LANGUAGE TemplateHaskell #-}

module Calc.Wasm.Allocator (moduleWithAllocator) where

import qualified Data.ByteString.Lazy as LB
import Data.FileEmbed
import qualified Language.Wasm as Wasm

-- these are saved in a file that is included in compilation
allocatorSource :: LB.ByteString
allocatorSource =
  LB.fromStrict $(makeRelativeToProject "static/bump-allocator.wat" >>= embedFile)

-- we have an allocator, we need to import it
moduleWithAllocator :: Wasm.Module
moduleWithAllocator = case Wasm.parse allocatorSource of
  Right mod' -> mod'
  Left e -> error (show e)
