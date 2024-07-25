{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE ImportQualifiedPost #-}

module Calc.Lsp (lsp) where

import Control.Monad.IO.Class
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        pure ()
    , requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover (InL ms) (Just range)
            ms = mkMarkdown "Hello world"
            range = Range pos pos
        responder (Right $ InL rsp)
    ]

lsp :: IO Int
lsp =
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = defaultOptions
      }
