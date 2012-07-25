module Graphics.UI.WebGUI.Events (
    Event (..)
  ) where

import {-# SOURCE #-} Graphics.UI.WebGUI.Properties.Properties
import {-# SOURCE #-} Graphics.UI.WebGUI.Properties.Props

data Event

instance Eq Event

instance Show Event
