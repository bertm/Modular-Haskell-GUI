-- | Tokens for GUI ~ Server communication

module Graphics.UI.WebGUI.Tokens (
    InputToken (..),
    OutputToken (..),
    
    Identifier,
    Time,
    Version,
    Value (..)
  ) where

import Data.Map

type Identifier    = Integer
type Time          = Integer
type Version       = String
data Value         = StringV String
                   | FloatV Float
                   | IntegerV Integer
                   | BoolV Bool
                   | ObjectV [(String, Value)]
                   | ListV [Value]
  deriving Show

-- | Input tokens, originating from the client
data InputToken
  = IEstablish
      Version     -- Client version
  | ISignal
      Identifier  -- Widget id
      String      -- Signal name
      Time        -- Signal time
      Value       -- Signal arguments
  | IKeepalive
  | IClose
  | IError 
      String      -- Error message
  | ISet 
      Identifier  -- Widget id
      String      -- Property name
      Value       -- Property value
  | IUnknown
  deriving Show

-- | Output tokens, to be sent to the client
data OutputToken
  = OAcknowledge 
      Version     -- Server version
  | OCreate 
      Identifier  -- Widget id
      String      -- Widget class
  | OAction 
      Identifier  -- Widget id
      String      -- Action name
      [Value]     -- Action arguments
  | OClose
  | OError
      String      -- Error message
  | OSet 
      Identifier  -- Widget id
      String      -- Property name
      Value       -- Property value

