{-# LANGUAGE
    NamedFieldPuns
  #-}

module Network.WebSockets.Simple.Logger where

import Network.WebSockets.Simple (WebSocketsApp (..), WebSocketsAppParams (..))
import Control.Monad.IO.Class (MonadIO, liftIO)


logStdout :: MonadIO m
          => Show receive
          => Show send
          => WebSocketsApp m receive send
          -> WebSocketsApp m receive send
logStdout WebSocketsApp{onOpen,onReceive,onClose} =
  WebSocketsApp
    { onOpen = onOpen . newParams
    , onReceive = \params x -> do
        liftIO $ putStrLn $ "Received: " ++ show x
        onReceive (newParams params) x
    , onClose
    }
  where
    newParams WebSocketsAppParams{send,close} =
      WebSocketsAppParams
        { send = \x -> do
            liftIO $ putStrLn $ "Sent: " ++ show x
            send x
        , close
        }
