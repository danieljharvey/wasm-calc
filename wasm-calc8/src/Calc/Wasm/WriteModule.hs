module Calc.Wasm.WriteModule (printModule, writeModule) where

import qualified Data.ByteString as BS
import qualified Language.Wasm.Binary as Wasm
import qualified Language.Wasm.Structure as Wasm

-- | in which we write some actual files somewhere for lols
writeModule :: FilePath -> Wasm.Module -> IO ()
writeModule path wasmMod = do
  let bs = Wasm.dumpModule wasmMod
  BS.writeFile path bs

-- | in which we output to stdout
printModule :: Wasm.Module -> IO ()
printModule = BS.putStr . Wasm.dumpModule
