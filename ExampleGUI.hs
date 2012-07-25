-- | An example GUI configuration.
-- This example starts a GUI server at port 9000, running the application logic that was
-- given as an argument to the startGUI function. When a connection is lost, the application
-- is terminated after 30 minutes.
-- This server communicates with the client using JSON over WebSockets.

module ExampleGUI (
    module GUI,
    startGUI
  ) where

import Server
import Server.Websocket
import JSON
import GUI

-- Starts the GUI server, given a function that will execute the application logic.
startGUI :: (Screen -> IO ()) -> IO ()
startGUI r = do s <- server r
                start s 9000

-- | Constructs a GUI server, given the function that will execute the application logic.
server :: (Screen -> IO ()) -> IO (Server String InputToken OutputToken)
server run = do c <- connections      -- Get the proper structure for storing connections.
                return $ makeServer
                          readToken   -- JSON reader.
                          writeToken  -- JSON writer.
                          (
                            processor -- The GUI.processor, doing the hard work.
                            run       -- The application logic to run.
                            c         -- Connection state.
                            1800      -- Timeout after connection loss.
                          )
