module Graphics.UI.WebGUI.Events.Protocol (
    toEvent,
    eventBitmask
  ) where

import Graphics.UI.WebGUI.Events

-- Converts an Event into the corresponding event name.
fromEvent :: Event -> String
fromEvent = show

-- | Converts an event name to the corresponding Event.
toEvent :: String -> Event
toEvent "Motion" = MotionEvent
toEvent "Scroll" = ScrollEvent
toEvent "Enter" = EnterEvent
toEvent "Leave" = LeaveEvent
toEvent "KeyPress" = KeyPressEvent
toEvent "KeyRelease" = KeyReleaseEvent
toEvent "ButtonPress" = ButtonPressEvent
toEvent "ButtonRelease" = ButtonReleaseEvent
toEvent "Focus" = FocusEvent
toEvent "Blur" = BlurEvent

-- | Returns the bitmask for a given Event.
eventBitmask :: Event -> Integer
eventBitmask e =
  case e of
    MotionEvent         -> 1
    ScrollEvent         -> 4
    EnterEvent          -> 16
    LeaveEvent          -> 64
    KeyPressEvent       -> 256
    KeyReleaseEvent     -> 1024
    ButtonPressEvent    -> 4096
    ButtonReleaseEvent  -> 16384
    FocusEvent          -> 65536
    BlurEvent           -> 262144
