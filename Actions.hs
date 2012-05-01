{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

module Actions (
    ActionAdd (..),
    ActionRemove (..)
  ) where

import Types

class ActionAdd a b where
    add :: a -> b -> IO ()

class ActionRemove a b where
    remove :: a -> b -> IO ()
