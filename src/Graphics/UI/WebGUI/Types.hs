{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}

module Graphics.UI.WebGUI.Types
  (
    EventObject (..)
  ) where

-- | An EventObject is an object o that can have a certain Event e.
class EventObject o e | o -> e where
    onEvent :: o -> e -> (e -> IO ()) -> IO ()
