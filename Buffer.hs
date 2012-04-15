-- | Buffers, inspired by <http://computationalthoughts.blogspot.com/2008/03/some-examples-of-software-transactional.html>
-- Additional reading on STM: <http://book.realworldhaskell.org/read/software-transactional-memory.html>
module Buffer
  (
    Buffer,
    newBuffer,
    bPut,
    bGet,
    bGet2,
    bPutIO,
    bGetIO,
    bGet2IO
  ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan

-- | A TVar buffer.
type Buffer a = TBChan a

-- | Creates an empty buffer.
newBuffer :: IO (Buffer a)
newBuffer = newTBChanIO 10

-- | Appends the given buffer with the given value.
bPut :: Buffer a -> a -> STM ()
bPut buffer item = writeTBChan buffer item

-- | Gets the first item in the buffer, blocking when empty.
bGet :: Buffer a -> STM a
bGet buffer = readTBChan buffer

-- | Gets the head of two buffers, blocking when both are empty.
bGet2 :: (Buffer a, Buffer b) -> STM (Maybe a, Maybe b)
bGet2 (a, b) =
  do r1 <- tryReadTBChan a
     r2 <- tryReadTBChan b
     case (r1, r2) of
       (Nothing, Nothing) -> retry
       _                  -> return (r1, r2)

-- IO versions of the above, to void the dependency on STM.
bPutIO b i = atomically $ bPut b i
bGetIO b = atomically $ bGet b
bGet2IO (a,b) = atomically $ bGet2 (a,b)
