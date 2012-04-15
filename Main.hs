module Main where

import Server
import Server.Websocket
import JSON

import GUI

main :: IO ()
main = start server 9000

server :: Server String InputToken OutputToken
server = makeServer readToken writeToken (processor getActions)



-- Main application logic

getActions :: Connection -> IO ()
getActions screen = do window <- newWindow screen
                       button <- newButton window
                       Label xx <- get button Label
                       print xx
                       set button Label "Hello"
                       set window Visible True
                       set button Visible True
                       Label xx <- get button Label
                       print xx
                       set button Events [ButtonReleaseEvent]
                       putStrLn "hi"
