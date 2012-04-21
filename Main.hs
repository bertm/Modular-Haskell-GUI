module Main where

import Server
import Server.Websocket
import JSON

import GUI

import Debug.Trace

main :: IO ()
main = start server 9000

server :: Server String InputToken OutputToken
server = makeServer readToken writeToken (processor run)



-- Main application logic

run :: Connection -> IO ()
run screen = do mainWindow <- newMainWindow screen
                set mainWindow Title "A GUI application"

                window <- newWindow screen
                box <- newBox window
                button <- newButton box
                entry <- newEntry box
                entry2 <- newEntry box
                
                -- Set some properties
                set window Title "Test Window"
                set button Label "Click me"
                set entry Text "Type here"
                set box Visible True
                set button Visible True
                set entry Visible True
                set entry2 Visible True
                
                -- Finally show window, to correctly center it
                set window Visible True
                
                -- Capture button releases on button
                set button Events [ButtonReleaseEvent]
                on button ButtonReleaseEvent $ const (do Text a <- get entry Text
                                                         set button Label ("You typed: " ++ a)
                                                         set entry Text "")
                -- Monitor for changes on entry text
                on entry (Change Text) $ const (do Text a <- get entry Text
                                                   set entry2 Text a)

