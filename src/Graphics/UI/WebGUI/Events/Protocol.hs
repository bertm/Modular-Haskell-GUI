module Graphics.UI.WebGUI.Events.Protocol (
    toEvent,
    eventBitmask
  ) where

import Graphics.UI.WebGUI.Events

-- Converts an Event into the corresponding event name.
fromEvent :: Event -> String
fromEvent MotionEvent = "motion"
fromEvent ScrollEvent = "scroll"
fromEvent EnterEvent = "enter"
fromEvent KeyPressEvent = "key-press"
fromEvent KeyReleaseEvent = "key-release"
fromEvent ButtonPressEvent = "button-press"
fromEvent ButtonReleaseEvent = "button-release"
fromEvent FocusEvent = "focus"
fromEvent BlurEvent = "blur"

-- | Converts an event name to the corresponding Event.
toEvent :: String -> Event
toEvent "motion" = MotionEvent
toEvent "scroll" = ScrollEvent
toEvent "enter" = EnterEvent
toEvent "leave" = LeaveEvent
toEvent "key-press" = KeyPressEvent
toEvent "key-release" = KeyReleaseEvent
toEvent "button-press" = ButtonPressEvent
toEvent "button-release" = ButtonReleaseEvent
toEvent "focus" = FocusEvent
toEvent "blur" = BlurEvent

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
