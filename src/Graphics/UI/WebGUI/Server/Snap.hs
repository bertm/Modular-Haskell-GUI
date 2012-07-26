{-# LANGUAGE OverloadedStrings #-}

-- | A WebSocket server implementation leveraging Snap to serve the files.
module Graphics.UI.WebGUI.Server.Snap
  (
    -- * Starting a server
    start
  ) where

import Network.WebSockets
import Network.WebSockets.Snap
import Control.Applicative
import Data.Monoid

import Snap.Core hiding (Request)
import Snap.Util.FileServe
import qualified Snap.Types.Headers as S
import qualified Snap.Internal.Http.Types as S
import Snap.Http.Server
import Snap.Http.Server.Config

import Graphics.UI.WebGUI.Server.Internal
import Graphics.UI.WebGUI.Server.Websocket (serveSocket)

-- | Starts the given @Server@ on the given port.
start :: (Eq s, Monoid s, WebSocketsData s) => Server s t1 t2 -- ^ The @Server@ to start.
                                            -> Int            -- ^ The port on which the server is to be bound.
                                            -> String         -- ^ The data directory to be served.
                                            -> IO ()
start server port dir = httpServe config $ run serve dir
  where serve = serveSocket (tokenizer server) (processor server)
        config = setPort port mempty

-- | A Snap that may open a WebSocket connection.
run :: (Request -> WebSockets Hybi10 ()) -> String -> Snap ()
run r d = do rq <- getRequest
             case S.lookup "upgrade" (S.rqHeaders rq) of
               Just _ -> runWebSocketsSnap r
               Nothing -> serveDirectory d
