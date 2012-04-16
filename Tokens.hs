-- | Tokens for GUI ~ Server communication

module Tokens (
    InputToken (..),
    OutputToken (..),
    
    Identifier,
    Time,
    Version
  ) where

type Identifier    = Int
type Time          = Integer
type Version       = String

-- | Input tokens, originating from the client
data InputToken
  = IEstablish
      Version    -- Client version
  | ISignal
      Identifier -- Widget id
      String     -- Signal name
      Time       -- Signal time
      [String]   -- Signal arguments
  | IKeepalive
  | IClose
  | IError 
      String     -- Error message
  | ISet 
      Identifier -- Widget id
      String     -- Property name
      String     -- Property value
  | IUnknown
  deriving Show

-- | Output tokens, to be sent to the client
data OutputToken
  = OAcknowledge 
      Version    -- Server version
  | OCreate 
      Identifier -- Widget id
      String     -- Widget class
  | OAction 
      Identifier -- Widget id
      String     -- Action name
      [String]   -- Action arguments
  | OClose
  | OError
      String     -- Error message
  | OSet 
      Identifier -- Widget id
      String     -- Property name
      String     -- Property value
