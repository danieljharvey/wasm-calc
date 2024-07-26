{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Lsp (lsp) where

import Data.Maybe
import Control.Monad.IO.Class
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

doLog :: (MonadIO m) => String -> m ()
doLog =
  liftIO . appendFile "/Users/danielharvey/git/wasm-calc/lsp-log.txt" . (<>) "\n"

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \notification -> do
        doLog (show notification)

        workspaceFolders <- fromMaybe [] <$> getWorkspaceFolders
        doLog ("workspaceFolders " <> show workspaceFolders)
          {-
        let opts =
              TextDocumentRegistrationOptions
                ( InL $
                    DocumentSelector
                      [ DocumentFilter (InL $ TextDocumentFilter (InR $ InR $ 
                            TextDocumentFilterPattern Nothing Nothing "*.calc"))
                      ]
                )
        _ <- registerCapability mempty SMethod_TextDocumentDidOpen opts $ \req -> do
          doLog "textDocumentDidOpen register response"
          doLog (show req) -}
        pure (),
      notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
        doLog "textDocumentDidOpen"
        doLog (show notification)
        pure (),
      notificationHandler SMethod_TextDocumentDidChange $ \notification -> do
        doLog "textDocumentDidChange"
        doLog (show notification)
        pure ()

    ]

lsp :: IO Int
lsp =
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right (),
        onConfigChange = const $ pure (),
        defaultConfig = (),
        configSection = "demo",
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = \_caps -> handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
            { optTextDocumentSync = Just syncOptions
            , optServerInfo = Just $ ServerInfo "Calc Language Server" Nothing
            }
      }
        where
          syncOptions = TextDocumentSyncOptions
                        (Just True) -- open/close notifications
                        (Just TextDocumentSyncKind_Full) -- changes
                        (Just False) -- will save
                        (Just False) -- will save (wait until requests are sent to server)
                        (Just $ InR $ SaveOptions $ Just False) -- save
