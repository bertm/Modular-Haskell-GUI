-- | Buffers, inspired by <http://computationalthoughts.blogspot.com/2008/03/some-examples-of-software-transactional.html>
-- Additional reading on STM: <http://book.realworldhaskell.org/read/software-transactional-memory.html>
module Buffer
  (
    Buffer,
    newBuffer,
    bind,
    bPut,
    bGet,
    bUnGet,
    bGet2,
    bindIO,
    bPutIO,
    bGetIO,
    bUnGetIO,
    bGet2IO
  ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBChan

-- | A TVar buffer.
type Buffer a = TVar (TBChan a)

-- | Creates an empty buffer.
newBuffer :: IO (Buffer a)
newBuffer = do c <- newTBChanIO 10
               newTVarIO c

-- | Binds the old buffer to the new buffer, discarding the contents of the old buffer.
bind :: Buffer a -> Buffer a -> STM ()
bind old new = do buf <- readTVar new
                  writeTVar old buf

-- | Appends the given buffer with the given value.
bPut :: Buffer a -> a -> STM ()
bPut buffer item = do buf <- readTVar buffer
                      writeTBChan buf item

-- | Gets the first item in the buffer, blocking when empty.
bGet :: Buffer a -> STM a
bGet buffer = do buf <- readTVar buffer
                 readTBChan buf

-- | Prepends the given buffer with the given value.
bUnGet :: Buffer a -> a -> STM ()
bUnGet buffer item = do buf <- readTVar buffer
                        unGetTBChan buf item

-- | Gets the head of two buffers, blocking when both are empty.
bGet2 :: (Buffer a, Buffer b) -> STM (Maybe a, Maybe b)
bGet2 (a, b) =
  do aa <- readTVar a
     bb <- readTVar b
     r1 <- tryReadTBChan aa
     r2 <- tryReadTBChan bb
     case (r1, r2) of
       (Nothing, Nothing) -> retry
       _                  -> return (r1, r2)

-- IO versions of the above, to void the dependency on STM.
bindIO o n = atomically $ bind o n
bPutIO b i = atomically $ bPut b i
bGetIO b = atomically $ bGet b
bUnGetIO b i = atomically $ bUnGet b i
bGet2IO (a,b) = atomically $ bGet2 (a,b)
