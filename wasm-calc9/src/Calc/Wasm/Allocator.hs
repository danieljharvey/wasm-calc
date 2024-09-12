{-# LANGUAGE TemplateHaskell #-}

module Calc.Wasm.Allocator (moduleWithAllocator) where

import qualified Data.ByteString as B
import Data.FileEmbed
import qualified Language.Wasm as Wasm

-- these are saved in a file that is included in compilation
allocatorSource :: B.ByteString
allocatorSource =
  $(makeRelativeToProject "static/malloc.wasm" >>= embedFile)

-- we have an allocator, we need to import it
moduleWithAllocator :: Wasm.Module
moduleWithAllocator = case Wasm.decode allocatorSource of
  Right mod' -> mod'
  Left e -> error (show e)
