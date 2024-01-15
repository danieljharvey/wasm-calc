{-# LANGUAGE OverloadedStrings #-}

module Test.RunNode
  (
    runScriptFromFile,
    lbsToString,
    withCache,
  )
where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Aeson             as JSON
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8)
import           System.Exit
import           System.Process.Typed

runProcessFromFile :: (MonadIO m) => String -> [String] -> String -> m (Bool, String)
runProcessFromFile binaryName args filename = do
  let allArgs = args <> [filename]
  result <- liftIO $ try $ readProcess (proc binaryName allArgs)
  case result of
    Right (ExitSuccess, success, _) ->
      pure (exitCodeToBool ExitSuccess, binNewline success)
    Right (ExitFailure exitCode, _, failure) ->
      pure (exitCodeToBool (ExitFailure exitCode), binNewline failure)
    Left e -> pure (False, show (e :: IOException))

-- | Pass a filepath to a JS file for Node to execute.
-- Required as ES modules don't work with the `-p` flag
runScriptFromFile :: (MonadIO m) => String -> String -> m (Bool, String)
runScriptFromFile path arg = runProcessFromFile "node" [arg] path

exitCodeToBool :: ExitCode -> Bool
exitCodeToBool ExitSuccess = True
exitCodeToBool _           = False

cacheResult :: (MonadIO m, JSON.ToJSON a) => String -> a -> m ()
cacheResult filename result = do
  let json = JSON.encode result
  liftIO $ LBS.writeFile filename json

-- load previously
loadCacheResult :: (MonadIO m, JSON.FromJSON a) => String -> m (Maybe a)
loadCacheResult filename = do
  res <- liftIO $ try $ LBS.readFile filename
  case (res :: Either IOError LBS.ByteString) of
    Right json -> do
      pure $ JSON.decode json
    Left _ -> pure Nothing

-- | Wrap a test in caching
withCache ::
  (MonadIO m, JSON.FromJSON a, JSON.ToJSON a) =>
  String ->
  m a ->
  m a
withCache cachePath action = do
  cached <- loadCacheResult cachePath
  case cached of
    Just res -> pure res
    Nothing -> do
      res <- action
      cacheResult cachePath res
      pure res

lbsToString :: LBS.ByteString -> String
lbsToString = T.unpack . decodeUtf8 . LBS.toStrict

binNewline :: LBS.ByteString -> String
binNewline = init . lbsToString
