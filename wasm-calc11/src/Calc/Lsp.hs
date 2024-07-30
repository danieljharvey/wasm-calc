{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Lsp (lsp) where

import Data.Text.Utf16.Rope.Mixed as TextRope
import qualified Data.Text as T
import Control.Lens hiding (Iso)
import qualified Language.LSP.VFS as LSP
import Data.Maybe
import Control.Monad.IO.Class
import Language.LSP.Protocol.Message
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import Language.LSP.Protocol.Lens qualified as LSP

doLog :: (MonadIO m) => String -> m ()
doLog =
  liftIO . appendFile "/Users/danielharvey/git/wasm-calc/lsp-log.txt" . (<>) "\n"

handlers :: LSP.Handlers (LSP.LspM ())
handlers =
  mconcat
    [ LSP.notificationHandler SMethod_Initialized $ \notification -> do
        doLog (show notification)

        workspaceFolders <- fromMaybe [] <$> LSP.getWorkspaceFolders
        doLog ("workspaceFolders " <> show workspaceFolders)
        pure (),
      LSP.notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
        doLog "textDocumentDidOpen"
        doLog (show notification)
        pure (),
      LSP.notificationHandler SMethod_TextDocumentDidChange $ \notification -> do
        -- doLog "textDocumentDidChange"
        -- doLog (show notification)
        let doc =
              notification
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        doLog ("Processing DidChangeTextDocument for: " <> show doc)
        let TNotificationMessage _ _
                (LSP.DidChangeTextDocumentParams
                    (LSP.VersionedTextDocumentIdentifier textDocumentIdentifier _) _)  = notification
        file <- findFile textDocumentIdentifier
        doLog (show file)
        pure (),
      LSP.notificationHandler SMethod_TextDocumentDidSave $ \notification -> do
        doLog "textDocumentDidSave"
        doLog (show notification)
        pure (),
      LSP.requestHandler SMethod_TextDocumentHover $ \req responder -> do
        doLog "textDocumentDidHover"
        doLog (show req)
        let TRequestMessage _ _ _ (LSP.HoverParams (LSP.TextDocumentIdentifier doc) pos _workDone) = req
        file <- findFile doc

        doLog (T.unpack file)

        let LSP.Position _l _c' = pos
            rsp = LSP.Hover (LSP.InL ms) (Just range)
            ms = LSP.mkMarkdown "Poo poo"
            range = LSP.Range pos pos
        responder (Right $ LSP.InL rsp)
    ]

findFile :: LSP.Uri -> LSP.LspM config T.Text
findFile doc = do
        let uri = LSP.toNormalizedUri doc
        mdoc <- LSP.getVirtualFile uri
        case mdoc of
          Just (LSP.VirtualFile _ _ str) -> do
            pure (TextRope.toText str)
          Nothing -> do
            error ("Didn't find anything in the VFS for: " <> show doc)


lsp :: IO Int
lsp =
  LSP.runServer $
    LSP.ServerDefinition
      { parseConfig = const $ const $ Right (),
        onConfigChange = const $ pure (),
        defaultConfig = (),
        configSection = "demo",
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = \_caps -> handlers,
        interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO,
        options = LSP.defaultOptions
            { LSP.optTextDocumentSync = Just syncOptions
            , LSP.optServerInfo = Just $ LSP.ServerInfo "Calc Language Server" Nothing
            }
      }
        where
          syncOptions = LSP.TextDocumentSyncOptions
                        (Just True) -- open/close notifications
                        (Just LSP.TextDocumentSyncKind_Full) -- changes
                        Nothing -- will save
                        Nothing -- will save (wait until requests are sent to server)
                        (Just $ LSP.InR $ LSP.SaveOptions $ Just False) -- save



