{-# OPTIONS_GHC -XMultiParamTypeClasses -XTypeSynonymInstances -XFlexibleInstances #-}

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

import {-# SOURCE #-} Graphics.UI.WebGUI.Properties.Props
import Graphics.UI.WebGUI.Properties.Types

data Visible
data Size
data Margin
data Sensitive
data CanFocus
data Title
data Opacity
data Label
data Parent
data Text
data Editable
data Visibility
data MaxLength
data Events
data Active
data Homogeneous
data Orientation

instance Show Visible
instance Show Size
instance Show Margin
instance Show Sensitive
instance Show CanFocus
instance Show Title
instance Show Opacity
instance Show Label
instance Show Parent
instance Show Text
instance Show Editable
instance Show Visibility
instance Show MaxLength
instance Show Events
instance Show Active
instance Show Homogeneous
instance Show Orientation

instance Property Visible VisibleT
instance Property Size SizeT
instance Property Margin MarginT
instance Property Sensitive SensitiveT
instance Property CanFocus CanFocusT
instance Property Title TitleT
instance Property Opacity OpacityT
instance Property Label LabelT
instance Property Text TextT
instance Property Editable EditableT
instance Property Visibility VisibilityT
instance Property MaxLength MaxLengthT
instance Property Events EventsT
instance Property Active ActiveT
instance Property Homogeneous HomogeneousT
instance Property Orientation OrientationT
