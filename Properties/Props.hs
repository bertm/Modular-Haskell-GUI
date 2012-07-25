-- | Defines a more generic wrapper around properties, making all of them of the same
-- type, regardless of the property.

module Properties.Props (
    Property (..),
    Prop (..),
    mergeProp,
    sameProp
  ) where

import {-# SOURCE #-} Properties.Properties

-- | Property definitions.
class Property p where
    toProp :: p -> Prop
    fromProp :: Prop -> p

-- A more or less generic property container
data Prop = VisibleProp Visible
          | SizeProp Size
          | MarginProp Margin
          | SensitiveProp Sensitive
          | CanFocusProp CanFocus
          | TitleProp Title
          | OpacityProp Opacity
          | LabelProp Label
          | ParentProp Parent
          | TextProp Text
          | EditableProp Editable
          | VisibilityProp Visibility
          | MaxLengthProp MaxLength
          | EventsProp Events
          | ActiveProp Active
          | HomogeneousProp Homogeneous
          | OrientationProp Orientation
  deriving Show

-- | Merges an old and a new Prop, usually ignoring the old one.
mergeProp :: Prop -> Prop -> Prop
mergeProp _ new = new

-- | Determines whether the two given Props are of the same type
sameProp :: Prop -> Prop -> Bool
sameProp (VisibleProp _) (VisibleProp _) = True
sameProp (SizeProp _) (SizeProp _) = True
sameProp (MarginProp _) (MarginProp _) = True
sameProp (SensitiveProp _) (SensitiveProp _) = True
sameProp (CanFocusProp _) (CanFocusProp _) = True
sameProp (TitleProp _) (TitleProp _) = True
sameProp (OpacityProp _) (OpacityProp _) = True
sameProp (LabelProp _) (LabelProp _) = True
sameProp (ParentProp _) (ParentProp _) = True
sameProp (TextProp _) (TextProp _) = True
sameProp (EditableProp _) (EditableProp _) = True
sameProp (VisibilityProp _) (VisibilityProp _) = True
sameProp (MaxLengthProp _) (MaxLengthProp _) = True
sameProp (EventsProp _) (EventsProp _) = True
sameProp (ActiveProp _) (ActiveProp _) = True
sameProp (HomogeneousProp _) (HomogeneousProp _) = True
sameProp (OrientationProp _) (OrientationProp _) = True
sameProp _ _ = False

