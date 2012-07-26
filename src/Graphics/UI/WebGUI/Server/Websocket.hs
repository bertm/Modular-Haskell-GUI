{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | A token consuming server using websockets to communicate.
module Graphics.UI.WebGUI.Server.Websocket
  (
    -- * Starting a server
    start,
    
    -- * External use
    serveSocket
  ) where

import Data.Text (Text)
import Network.WebSockets
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS

import Graphics.UI.WebGUI.Server.Internal
import Graphics.UI.WebGUI.Buffer


instance WebSocketsData String where
    fromLazyByteString = BS.unpack . fromLazyByteString
    toLazyByteString   = toLazyByteString . BS.pack

-- | Starts the given @Server@ on the given port.
start :: (Eq s, Monoid s, WebSocketsData s) => Server s t1 t2 -- ^ The @Server@ to start.
                                            -> Int            -- ^ The port on which the server is to be bound.
                                            -> IO ()
start server port = runServer "0.0.0.0" port $ serve
  where serve = serveSocket (tokenizer server) (processor server)

-- | Serves a websocket.
serveSocket :: (Eq s, Monoid s, WebSocketsData s) => Tokenizer s t1 t2 -- ^ The @Tokenizer@ to use.
                                                  -> Processor t1 t2   -- ^ The @Processor@ to use.
                                                  -> Request           -- ^ The Request to handle.
                                                  -> WebSockets Hybi10 ()
serveSocket t p rq =
  do acceptRequest rq
     sink <- getSink
     inp <- liftIO (serveSocketIO sink)
     produceInput inp
  where serveSocketIO sink = do inp <- newBuffer
                                out <- newBuffer
                                outputThread <- forkIO $ produceOutput sink out
                                processThread <- forkIO $ (process p) inp out
                                return inp
        parse s = case readToken t s of
                    (t, s') -> t : if (s' /= mempty) then parse s' else []
        produceOutput sink out = do token <- atomically $ bGet out
                                    sendSink sink . textData . writeToken t $ token
                                    produceOutput sink out
        produceInput inp = do msg <- receiveData
                              liftIO (mapM_ (atomically . bPut inp) (parse msg))
                              produceInput inp
