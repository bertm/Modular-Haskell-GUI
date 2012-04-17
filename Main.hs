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
run screen = do window <- newWindow screen
                button <- newButton window
                entry <- newEntry window
                entry2 <- newEntry window
                
                -- Set some properties
                set button Label "Click me"
                set entry Text "Type here"
                set window Visible True
                set button Visible True
                set entry Visible True
                set entry2 Visible True
                
                -- Capture button releases on button
                set button Events [ButtonReleaseEvent]
                on button ButtonReleaseEvent $ const (do Text a <- get entry Text
                                                         set button Label ("You typed: " ++ a)
                                                         set entry Text "")
                -- Monitor for changes on entry text
                on entry (Change Text) $ const (do Text a <- get entry Text
                                                   set entry2 Text a)

