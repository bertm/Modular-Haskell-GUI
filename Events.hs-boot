module Events (
    Event (..)
  ) where

import {-# SOURCE #-} Properties.Properties
import {-# SOURCE #-} Properties.Props

data Event

instance Eq Event

instance Show Event
