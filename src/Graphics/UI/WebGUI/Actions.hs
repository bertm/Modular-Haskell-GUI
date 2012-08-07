{-# OPTIONS_GHC -XMultiParamTypeClasses #-}

-- | Defines Actions that can be performed on certain Widgets.
module Graphics.UI.WebGUI.Actions (
    ActionAddRemove (..),
    ActionShowHide (..),
    ActionEvents (..),
    ActionDestroy (..)
  ) where

import Graphics.UI.WebGUI.Events

-- | Allows to add a Widget to another Widget, or remove it.
class ActionAddRemove parent child where
    add :: parent -> child -> IO ()
    remove :: parent -> child -> IO ()

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
