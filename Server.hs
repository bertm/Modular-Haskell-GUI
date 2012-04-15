
module Server
  (
    -- * The Server type
    Server,
    -- * Defining servers
    makeServer,
    TokenReader,
    TokenWriter,
    TokenProcessor,
    -- * Example servers
    echoServer,
    lineEchoServer
  ) where

import Data.Monoid
import Control.Concurrent.STM

import Buffer
import Server.Internal

-- | A simple server that immediately echoes its input.
echoServer :: (Monoid t) => Server t t t
echoServer = Server identityTokenizer identityProcessor

-- | A simple server that echoes each line it receives.
lineEchoServer :: Server String String String
lineEchoServer = Server lineStringTokenizer identityProcessor

-- | The identity tokenizer, delivering the entire input as a token, outputting the processor output directly.
identityTokenizer :: Monoid t => Tokenizer t t t
identityTokenizer = Tokenizer (\s -> (s, mempty)) id

-- | A line tokenizer, deliviring each received line as a token, outputting the processor output as lines.
lineStringTokenizer :: Tokenizer String String String
lineStringTokenizer = Tokenizer readToken writeToken
  where readToken s = case break (== '\n') s of (a, b) -> case null b || head b == '\n' of True -> (a, drop 1 b)
        writeToken  = flip (++) "\n"

-- | The identity processor, outputting all tokens it receives.
identityProcessor :: Processor t t
identityProcessor = Processor id
  where id inp out = do var <- atomically $ bGet inp
                        atomically $ bPut out var
                        id inp out

