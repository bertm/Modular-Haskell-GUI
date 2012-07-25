-- | Defines all available properties that may be assigned to widgets.

module Graphics.UI.WebGUI.Properties.Properties (
    Visible (..),
    Size (..),
    Margin (..),
    Sensitive (..),
    CanFocus (..),
    Title (..),
    Opacity (..),
    Label (..),
    Parent (..),
    Text (..),
    Editable (..),
    Visibility (..),
    MaxLength (..),
    Events (..),
    Active (..),
    Homogeneous (..),
    Orientation (..)
  ) where

import Graphics.UI.WebGUI.Properties.Props
import {-# SOURCE #-} Graphics.UI.WebGUI.Events
    
-- Basic property definition.
data Visible = Visible Bool
  deriving Show
data Size = Size (Integer, Integer)
  deriving Show
data Margin = Margin (Integer, Integer, Integer, Integer)
  deriving Show
data Sensitive = Sensitive Bool
  deriving Show
data CanFocus = CanFocus Bool
  deriving Show
data Title = Title String
  deriving Show
data Opacity = Opacity Float
  deriving Show
data Label = Label String
  deriving Show
data Parent = Parent Integer -- TODO: remove?
  deriving Show
data Text = Text String
  deriving Show
data Editable = Editable Bool
  deriving Show
data Visibility = Visibility Bool
  deriving Show
data MaxLength = MaxLength Integer
  deriving Show
data Events = Events [Event]
  deriving Show
data Active = Active Bool
  deriving Show
data Homogeneous = Homogeneous Bool
  deriving Show
data Orientation = Orientation String -- TODO: enum
  deriving Show

-- Instances for forcing properties in a more generic Prop container.
instance Property Visible
  where toProp = VisibleProp
        fromProp (VisibleProp v) = v
instance Property Size
  where toProp = SizeProp
        fromProp (SizeProp v) = v
instance Property Margin
  where toProp = MarginProp
        fromProp (MarginProp v) = v
instance Property Sensitive
  where toProp = SensitiveProp
        fromProp (SensitiveProp v) = v
instance Property CanFocus
  where toProp = CanFocusProp
        fromProp (CanFocusProp v) = v
instance Property Title
  where toProp = TitleProp
        fromProp (TitleProp v) = v
instance Property Opacity
  where toProp = OpacityProp
        fromProp (OpacityProp v) = v
instance Property Label
  where toProp = LabelProp
        fromProp (LabelProp v) = v
instance Property Text
  where toProp = TextProp
        fromProp (TextProp v) = v
instance Property Editable
  where toProp = EditableProp
        fromProp (EditableProp v) = v
instance Property Visibility
  where toProp = VisibilityProp
        fromProp (VisibilityProp v) = v
instance Property MaxLength
  where toProp = MaxLengthProp
        fromProp (MaxLengthProp v) = v
instance Property Events
  where toProp = EventsProp
        fromProp (EventsProp v) = v
instance Property Active
  where toProp = ActiveProp
        fromProp (ActiveProp v) = v
instance Property Homogeneous
  where toProp = HomogeneousProp
        fromProp (HomogeneousProp v) = v
instance Property Orientation
  where toProp = OrientationProp
        fromProp (OrientationProp v) = v
