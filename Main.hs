module Main where

import Server
import Server.Websocket
import JSON

import GUI

import Debug.Trace

main :: IO ()
main = do s <- server
          start s 9000

server :: IO (Server String InputToken OutputToken)
server = do c <- connections
            return $ makeServer readToken writeToken (processor run c)



-- Main application logic

run :: Connection -> IO ()
run conn = do mainWindow <- newMainWindow conn
              set mainWindow Title "A GUI application"

              window <- newWindow conn
              box <- newBox conn
              button <- newButton conn
              entry <- newEntry conn
              entry2 <- newEntry conn
              
              add window box
              add box button
              add box entry
              add box entry2
              
              -- Set some properties
              set window Title "Test Window"
              set button Label "Click me"
              set entry Text "Type here"
              set button CanFocus True
              set entry CanFocus True
              set entry2 CanFocus True
              set box Visible True
              set button Visible True
              set entry Visible True
              set entry2 Visible True
              
              set box Orientation "vertical"
              
              -- Finally show window, to correctly center it
              set window Visible True
              
              -- Add a button to the main window
              button2 <- newButton conn
              set button2 Visible True
              set button2 Label "Main button label"
              set button2 CanFocus True
              set mainWindow Visible True
              
              add mainWindow button2
              
              -- Capture button releases on button
              enableEvents button [ButtonReleaseEvent, ButtonPressEvent, FocusEvent, BlurEvent]
              on button (Change Active) $ const (do Text a <- get entry Text
                                                    set button Label ("You typed: " ++ a)
                                                    set entry Text "")
              -- Monitor for changes on entry text
              on entry (Change Text) $ const (do Text a <- get entry Text
                                                 set entry2 Text a)

