-- | An example GUI configuration.
-- This example starts a GUI server at port 9000, running the application logic that was
-- given as an argument to the startGUI function. When a connection is lost, the application
-- is terminated after 30 minutes.
-- This server communicates with the client using JSON over WebSockets.

module Snap (
    main
  ) where

import Graphics.UI.WebGUI.Server
import Graphics.UI.WebGUI.Server.Snap
import Graphics.UI.WebGUI.JSON
import Graphics.UI.WebGUI.GUI

main = startGUI logic

-- Starts the GUI server, given a function that will execute the application logic.
startGUI :: (Screen -> IO ()) -> IO ()
startGUI r = do s <- server r
                start s 9000 "../data/Client"

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
                          
logic :: Screen -> IO ()
logic screen = do mainWindow <- newMainWindow screen
                  set mainWindow [Title := "A GUI application"]

                  window <- newWindow screen
                  box <- newBox screen
                  button <- newButton screen
                  entry <- newLineEdit screen
                  entry2 <- newLineEdit screen
                
                  add window box
                  add box button
                  add box entry
                  add box entry2
                
                  -- Set some properties
                  set window [Title := "Test Window"]
                  set button [Label := "Click me",
                              CanFocus := True,
                              Visible := True]
                  set entry [Text := "Type here",
                             CanFocus := True,
                             Visible := True]
                  set entry2 [CanFocus := True,
                              Visible := True]
                  set box [Visible := True,
                           Orientation := VerticalOrientation]
                 
                  -- Finally show window, to correctly center it
                  set window [Visible := True]
                
                  -- Add a button to the main window
                  button2 <- newButton screen
                  set button2 [Visible := True,
                               Label := "Main button label"]

                  set mainWindow [Visible := True]
                
                  add mainWindow button2
                
                  -- Capture button releases on button
                  enableEvents button [ButtonReleaseEvent]
                  onEvent button ButtonReleaseEvent $ const (do a <- get entry Text
                                                                set button [Label := ("You typed: " ++ a)]
                                                                set entry [Text := ""])
                  -- Monitor for changes on entry text
                  onChange entry Text $ do a <- get entry Text
                                           case a of
                                             "quit" -> quit screen
                                             _      -> set entry2 [Text := a]
                  
                  trickButton <- newButton screen
                  add box trickButton
                  set trickButton [Visible := True,
                                   Label := "Get rid of me"]
                  enableEvents trickButton [ButtonReleaseEvent]
                  onEvent trickButton ButtonReleaseEvent $ const (remove box trickButton)
