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

instance Property Visible
instance Property Size
instance Property Margin
instance Property Sensitive
instance Property CanFocus
instance Property Title
instance Property Opacity
instance Property Label
instance Property Text
instance Property Editable
instance Property Visibility
instance Property MaxLength
instance Property Events
instance Property Active
instance Property Homogeneous
instance Property Orientation
