{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | A token consuming server using raw TCP sockets to communicate.
module Graphics.UI.WebGUI.Server.Raw
  (
    ReadWrite(..),
    -- * Starting a server
    start
  ) where

import Network
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Concurrent.STM
import Data.Monoid

import Graphics.UI.WebGUI.Server.Internal
import Graphics.UI.WebGUI.Buffer

-- | Any type you want to read or write to the socket must be an instance of the @ReadWrite@ class.
class (Monoid a, Eq a) => ReadWrite a where
  -- | Gets all contents from the handle in a lazy way.
  hGetAll :: Handle -> IO a
  
  -- | Puts all contents to the handle.
  hPutAll :: Handle -> a -> IO ()

-- | The simple @ReadWrite@ instance for String, enabling Strings to be read and written by the server.
instance ReadWrite String where
  hGetAll = hGetContents
  hPutAll = hPutStr

type Connection = (Handle, HostName, PortNumber)

-- | Starts the given @Server@ on the given port.
start :: ReadWrite s => Server s t1 t2 -- ^ The @Server@ to start.
                     -> PortNumber     -- ^ The port on which the server is to be bound.
                     -> IO ()
start server port = withSocketsDo $ bracket open close serve
  where open  = listenOn $ PortNumber port
        close = sClose
        serve = serveSocket (tokenizer server) (processor server)

-- | Accepts connections from clients.
serveSocket :: ReadWrite s => Tokenizer s t1 t2 -- ^ The @Tokenizer@ to use.
                           -> Processor t1 t2   -- ^ The @Processor@ to use.
                           -> Socket            -- ^ The Socket to accept connections on.
                           -> IO ()
serveSocket t p socket = serveSocket'
  where serveSocket' = do conn <- accept socket
                          connectionThread <- forkIO $ bracket (return conn) close handle
                          serveSocket' `finally` killThread connectionThread
        handle conn  = handleConnection t p conn
        close        = (\(h, _, _) -> hClose h)

-- | Handles an incoming connection.
handleConnection :: ReadWrite s => Tokenizer s t1 t2 -- ^ The @Tokenizer@ to use.
                                -> Processor t1 t2   -- ^ The @Processor@ to use.
                                -> Connection        -- ^ The Connection to serve.
                                -> IO ()
handleConnection t p (h, _, _) =
  do hSetBuffering h NoBuffering
     s <- hGetAll h
     inp <- newBuffer
     out <- newBuffer
     inputThread <- forkIO $ produceInput inp . parse $ s
     processThread <- forkIO $ (process p) inp out
     produceOutput out `finally` mapM_ killThread [inputThread, processThread]
  where parse s = case readToken t s of
                    (t, s') -> t : if (s' /= mempty) then parse s' else []
        produceOutput out = do token <- atomically $ bGet out
                               hPutAll h . writeToken t $ token
                               produceOutput out
        produceInput inp ts = mapM_ (atomically . bPut inp) ts
