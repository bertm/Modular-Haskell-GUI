-- | This module defines the events that can occur in the GUI.

module Graphics.UI.WebGUI.Events (
    Event (..)
  ) where

import {-# SOURCE #-} Graphics.UI.WebGUI.Properties.Properties
import {-# SOURCE #-} Graphics.UI.WebGUI.Properties.Props

-- | Define all events that can occur.
data Event = MotionEvent
           | ScrollEvent
           | EnterEvent
           | LeaveEvent
           | KeyPressEvent
           | KeyReleaseEvent
           | ButtonPressEvent
           | ButtonReleaseEvent
           | FocusEvent
           | BlurEvent
           | ChangeEvent Prop

instance Eq Event where
  MotionEvent == MotionEvent = True
  ScrollEvent == ScrollEvent = True
  EnterEvent == EnterEvent = True
  LeaveEvent == LeaveEvent = True
  KeyPressEvent == KeyPressEvent = True
  KeyReleaseEvent == KeyReleaseEvent = True
  ButtonPressEvent == ButtonPressEvent = True
  ButtonReleaseEvent == ButtonReleaseEvent = True
  FocusEvent == FocusEvent = True
  BlurEvent == BlurEvent = True
  ChangeEvent a == ChangeEvent b = sameProp a b
  _ == _ = False

instance Show Event where
  show MotionEvent = "MotionEvent"
  show ScrollEvent = "ScrollEvent"
  show EnterEvent = "EnterEvent"
  show KeyPressEvent = "KeyPressEvent"
  show KeyReleaseEvent = "KeyReleaseEvent"
  show ButtonPressEvent = "ButtonPressEvent"
  show ButtonReleaseEvent = "ButtonReleaseEvent"
  show FocusEvent = "FocusEvent"
  show BlurEvent = "BlurEvent"
  show (ChangeEvent _) = "ChangeEvent"
