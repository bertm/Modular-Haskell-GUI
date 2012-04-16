-- A JSON implementation of the protocol for our remote GUI

module JSON (readToken, writeToken) where

import Text.JSON (JSON, JSValue)
import qualified Text.JSON as JS

import Debug.Trace

-- We need Server for the definition of TokenReader and TokenWriter
import Server
import Tokens

-- Making OutputToken instance of JSON so we can write OutputTokens to the client in JSON-style
instance JSON OutputToken where
  readJSON = error "Output tokens should not be read."
  showJSON (OAcknowledge version) = JS.makeObj [("type", JS.showJSON "acknowledge"), ("version", JS.showJSON version)]
  showJSON (OCreate id cls)       = JS.makeObj [("type", JS.showJSON "create"), ("class", JS.showJSON cls), ("id", JS.showJSON id)]
  showJSON (OAction id name args) = JS.makeObj [("type", JS.showJSON "action"), ("name", JS.showJSON name), ("id", JS.showJSON id), ("args", JS.showJSON args)]
  showJSON (OSet id name val)     = JS.makeObj [("type", JS.showJSON "set"), ("name", JS.showJSON name), ("id", JS.showJSON id), ("value", JS.showJSON val)]
  showJSON (OError msg)           = JS.makeObj [("type", JS.showJSON "error"), ("msg", JS.showJSON msg)]
  showJSON OClose                 = JS.makeObj [("type", JS.showJSON "close")]

-- Making InputToken instance of JSON so we can read InputTokens from the JSON-text input stream.
instance JSON InputToken where
  readJSON (JS.JSObject o) = trace (show o) (case JS.valFromObj "type" o of
                               -- Establish token
                               JS.Ok "establish" -> case JS.valFromObj "version" o of
                                                      JS.Ok version -> JS.Ok $ IEstablish version
                                                      _ -> JS.Ok IUnknown
                               -- Signal token
                               JS.Ok "signal" -> case (JS.valFromObj "name" o, JS.valFromObj "id" o, JS.valFromObj "time" o, JS.valFromObj "args" o) of
                                                        (JS.Ok name, JS.Ok id, JS.Ok time, JS.Ok args) -> JS.Ok $ ISignal id name time args
                                                        _ -> JS.Ok IUnknown
                               -- Keepalive token
                               JS.Ok "keepalive" -> JS.Ok IKeepalive
                               -- Close token
                               JS.Ok "close" -> JS.Ok IClose
                               -- Error token
                               JS.Ok "error" -> case JS.valFromObj "msg" o of
                                                      JS.Ok msg -> JS.Ok $ IError msg
                                                      _ -> JS.Ok IUnknown
                               -- Set token
                               JS.Ok "set" -> case (JS.valFromObj "id" o, JS.valFromObj "name" o, JS.valFromObj "value" o) of
                                                        (JS.Ok id, JS.Ok name, JS.Ok value) -> JS.Ok $ ISet id name value
                                                        _ -> JS.Ok IUnknown
                               _          -> JS.Ok IUnknown)
  showJSON = error "Input tokens should not be written."

debug = flip const
--debug = trace

-- The tokenizer for the JSON protocol implementation
readToken :: TokenReader String InputToken
readToken s = debug "read" (case JS.decode s of
                JS.Ok a    -> (a, [])
                JS.Error a -> error a)

-- The token writer for the JSON protocol implementation
writeToken :: TokenWriter String OutputToken
writeToken t = debug (t `seq` "write") JS.encode t
