module Graphics.UI.WebGUI.Server.Internal where

import Data.Monoid
import Control.Concurrent.STM

import Graphics.UI.WebGUI.Buffer

-- | The functional definition of the server.
data Server s t1 t2 = Server
  { tokenizer :: Tokenizer s t1 t2 -- ^ Converts the input to tokens and tokens to output.
  , processor :: Processor t1 t2   -- ^ Processes all tokens.
  }
  
-- | Reading and writing definition of tokens.
data Tokenizer s t1 t2 = Tokenizer
  { readToken :: TokenReader s t1  -- ^ Converts all input to tokens.
  , writeToken :: TokenWriter s t2 -- ^ Converts all tokens to output.
  }

-- | Processor definition.
data Processor t1 t2 = Processor
  { process :: TokenProcessor t1 t2 -- ^ Processes all tokens, thereby handling the data flow.
  }

-- | A reader for tokens.
type TokenReader s t1 = s       -- ^ The input as read from the socket.
                     -> (t1, s) -- ^ A token and the remaining input.
                    
-- | A writer for tokens.
type TokenWriter s t2 = t2 -- ^ The token to be written.
                     -> s  -- ^ The output to be written to the socket.

-- | A processor, reading tokens from the socket and writing tokens to the socket.
type TokenProcessor t1 t2 = Buffer t1 -- ^ The input tokens that are read from the socket.
                         -> Buffer t2 -- ^ The output tokens that are to be written.
                         -> IO ()

-- | Creates a @Server@ definition from the token reader, writer and processor definitions.
makeServer :: TokenReader s t1     -- ^ The @TokenReader@ responsible for reading the input tokens.
           -> TokenWriter s t2     -- ^ The @TokenWriter@ responsible for writing the output tokens.
           -> TokenProcessor t1 t2 -- ^ The @TokenProcessor@ responsible for the application logic.
           -> Server s t1 t2       -- ^ The resulting @Server@.
makeServer r w p = Server tokenizer processor
  where tokenizer = Tokenizer r w
        processor = Processor p
