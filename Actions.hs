{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

-- | Defines Actions that can be performed on certain Widgets.
module Actions (
    ActionAddRemove (..),
    ActionShowHide (..),
    ActionEvents (..),
    ActionDestroy (..)
  ) where

import Types
import Properties

-- | Allows to add a Widget to another Widget, or remove it.
class ActionAddRemove a b where
    add :: a -> b -> IO ()
    remove :: a -> b -> IO ()

-- | Allows to show a Widget, or hide it again.
class ActionShowHide a where
    showSingle :: a -> IO ()
    showAll :: a -> IO ()
    hideSingle :: a -> IO ()
    hideAll :: a -> IO ()

-- | Enables or disables propagation of events.
class ActionEvents a where
    enableEvents :: a -> [Event] -> IO ()
    disableEvents :: a -> [Event] -> IO ()

-- | Destroys a Widget.
class ActionDestroy a where
    destroy :: a -> IO ()
