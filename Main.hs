module Main where

import Server
import Server.Websocket
import JSON

import GUI

main :: IO ()
main = start server 9000

server :: Server String InputToken OutputToken
server = makeServer readToken writeToken (processor run)



-- Main application logic

run :: Connection -> IO ()
run screen = do window <- newWindow screen
                button <- newButton window
                entry <- newEntry window
                Label xx <- get button Label
                set button Label "Click me"
                set window Visible True
                set button Visible True
                Label xx <- get button Label
                set button Events [ButtonReleaseEvent]
                on button ButtonReleaseEvent (\x -> do Text a <- get entry Text
                                                       set button Label ("You typed: " ++ a)
                                                       set entry Text "")
