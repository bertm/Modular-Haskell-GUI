{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
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
    Orientation (..), OrientationT (..)
  ) where

import Graphics.UI.WebGUI.Properties.Props
import Graphics.UI.WebGUI.Properties.Types
    
-- Basic property definition.
data Visible = Visible VisibleT
  deriving Show
data Size = Size SizeT
  deriving Show
data Margin = Margin MarginT
  deriving Show
data Sensitive = Sensitive SensitiveT
  deriving Show
data CanFocus = CanFocus CanFocusT
  deriving Show
data Title = Title TitleT | OtherTitle Int
  deriving Show
data Opacity = Opacity OpacityT
  deriving Show
data Label = Label LabelT
  deriving Show
data Parent = Parent ParentT
  deriving Show
data Text = Text TextT
  deriving Show
data Editable = Editable EditableT
  deriving Show
data Visibility = Visibility VisibilityT
  deriving Show
data MaxLength = MaxLength MaxLengthT
  deriving Show
data Events = Events EventsT
  deriving Show
data Active = Active ActiveT
  deriving Show
data Homogeneous = Homogeneous HomogeneousT
  deriving Show
data Orientation = Orientation OrientationT
  deriving Show


-- Instances for forcing properties in a more generic Prop container.
instance Property Visible VisibleT
  where toValue (Visible v) = v
        toProp = VisibleProp
        fromProp (VisibleProp v) = v
instance Property Size SizeT
  where toValue (Size v) = v
        toProp = SizeProp
        fromProp (SizeProp v) = v
instance Property Margin MarginT
  where toValue (Margin v) = v
        toProp = MarginProp
        fromProp (MarginProp v) = v
instance Property Sensitive SensitiveT
  where toValue (Sensitive v) = v
        toProp = SensitiveProp
        fromProp (SensitiveProp v) = v
instance Property CanFocus CanFocusT
  where toValue (CanFocus v) = v
        toProp = CanFocusProp
        fromProp (CanFocusProp v) = v
instance Property Title TitleT
  where toValue (Title v) = v
        toProp = TitleProp
        fromProp (TitleProp v) = v
instance Property Opacity OpacityT
  where toValue (Opacity v) = v
        toProp = OpacityProp
        fromProp (OpacityProp v) = v
instance Property Label LabelT
  where toValue (Label v) = v
        toProp = LabelProp
        fromProp (LabelProp v) = v
instance Property Parent ParentT
  where toValue (Parent v) = v
        toProp = ParentProp
        fromProp (ParentProp v) = v
instance Property Text TextT
  where toValue (Text v) = v
        toProp = TextProp
        fromProp (TextProp v) = v
instance Property Editable EditableT
  where toValue (Editable v) = v
        toProp = EditableProp
        fromProp (EditableProp v) = v
instance Property Visibility VisibilityT
  where toValue (Visibility v) = v
        toProp = VisibilityProp
        fromProp (VisibilityProp v) = v
instance Property MaxLength MaxLengthT
  where toValue (MaxLength v) = v
        toProp = MaxLengthProp
        fromProp (MaxLengthProp v) = v
instance Property Events EventsT
  where toValue (Events v) = v
        toProp = EventsProp
        fromProp (EventsProp v) = v
instance Property Active ActiveT
  where toValue (Active v) = v
        toProp = ActiveProp
        fromProp (ActiveProp v) = v
instance Property Homogeneous HomogeneousT
  where toValue (Homogeneous v) = v
        toProp = HomogeneousProp
        fromProp (HomogeneousProp v) = v
instance Property Orientation OrientationT
  where toValue (Orientation v) = v
        toProp = OrientationProp
        fromProp (OrientationProp v) = v
