module Graphics.UI.WebGUI.Properties.Types where

import {-# SOURCE #-} Graphics.UI.WebGUI.Events
import Graphics.UI.WebGUI.Tokens

type VisibleT = Bool
type SizeT = (Integer, Integer)
type MarginT = (Integer, Integer, Integer, Integer)
type SensitiveT = Bool
type CanFocusT = Bool
type TitleT = String
type OpacityT = Float
type LabelT = String
type ParentT = Maybe Identifier
type TextT = String
type EditableT = Bool
type VisibilityT = Bool
type MaxLengthT = Integer
type EventsT = [Event]
type ActiveT = Bool
type HomogeneousT = Bool
data OrientationT = HorizontalOrientation
                  | VerticalOrientation
  deriving Show
