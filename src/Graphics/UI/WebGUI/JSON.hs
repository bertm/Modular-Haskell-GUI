-- A JSON implementation of the protocol for our remote GUI

module Graphics.UI.WebGUI.JSON (readToken, writeToken) where

import Text.JSON (JSON, JSValue)
import qualified Text.JSON as JS

import Debug.Trace

-- We need Server for the definition of TokenReader and TokenWriter
import Graphics.UI.WebGUI.Server
import Graphics.UI.WebGUI.Tokens

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
  readJSON (JS.JSObject o) = debug (show o) (case JS.valFromObj "type" o of
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
  readJSON other = error $ "Unhandled input token: " ++ show other
  showJSON = error "Input tokens should not be written."


instance JSON Value where
  showJSON (StringV v) = JS.showJSON v
  showJSON (FloatV v) = JS.showJSON v
  showJSON (IntegerV v) = JS.showJSON v
  showJSON (BoolV v) = JS.showJSON v
  showJSON (ObjectV v) = JS.encJSDict v
  showJSON (ListV v) = JS.showJSON v
  readJSON v@(JS.JSObject _) = fmap ObjectV $ JS.decJSDict "ObjectV" v
  readJSON v@(JS.JSArray _) = fmap ListV $ JS.readJSONs v
  readJSON v@(JS.JSString _) = fmap StringV $ JS.readJSON v
  readJSON (JS.JSRational True v) = JS.Ok $ FloatV $ fromRational v
  readJSON (JS.JSRational False v) = JS.Ok $ IntegerV $ round v
  readJSON (JS.JSBool v) = JS.Ok $ BoolV v
  readJSON JS.JSNull = JS.Ok $ ObjectV []

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
