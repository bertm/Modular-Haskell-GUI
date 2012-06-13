{-# OPTIONS_GHC -XMultiParamTypeClasses -XExistentialQuantification -XFlexibleContexts #-}

-- | Definition of all possible properties widgets may or may not have.

module Properties (
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
    Event (..),
    Active (..),
    Homogeneous (..),
    Orientation (..),
    
    Prop (..),
    sameProp
  ) where

import Types

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
           | forall a p. Property p Prop => Change (a -> p)
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
  Change a == Change b = sameProp (toProp $ a undefined) (toProp $ b undefined)
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
  show (Change _) = "Change"
  show (ChangeEvent _) = "ChangeEvent"

-- Basic property definition
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

-- Instances for forcing properties in a more generic Prop container
instance Property Visible Prop
  where toProp = VisibleProp
        get o a = unsafeGet o Visible >>= (\(VisibleProp v) -> return v)
instance Property Size Prop
  where toProp = SizeProp
        get o a = unsafeGet o Size >>= (\(SizeProp v) -> return v)
instance Property Margin Prop
  where toProp = MarginProp
        get o a = unsafeGet o Margin >>= (\(MarginProp v) -> return v)
instance Property Sensitive Prop
  where toProp = SensitiveProp
        get o a = unsafeGet o Sensitive >>= (\(SensitiveProp v) -> return v)
instance Property CanFocus Prop
  where toProp = CanFocusProp
        get o a = unsafeGet o CanFocus >>= (\(CanFocusProp v) -> return v)
instance Property Title Prop
  where toProp = TitleProp
        get o a = unsafeGet o Title >>= (\(TitleProp v) -> return v)
instance Property Opacity Prop
  where toProp = OpacityProp
        get o a = unsafeGet o Opacity >>= (\(OpacityProp v) -> return v)
instance Property Label Prop
  where toProp = LabelProp
        get o a = unsafeGet o Label >>= (\(LabelProp v) -> return v)
instance Property Text Prop
  where toProp = TextProp
        get o a = unsafeGet o Text >>= (\(TextProp v) -> return v)
instance Property Editable Prop
  where toProp = EditableProp
        get o a = unsafeGet o Editable >>= (\(EditableProp v) -> return v)
instance Property Visibility Prop
  where toProp = VisibilityProp
        get o a = unsafeGet o Visibility >>= (\(VisibilityProp v) -> return v)
instance Property MaxLength Prop
  where toProp = MaxLengthProp
        get o a = unsafeGet o MaxLength >>= (\(MaxLengthProp v) -> return v)
instance Property Events Prop
  where toProp = EventsProp
        get o a = unsafeGet o Events >>= (\(EventsProp v) -> return v)
instance Property Active Prop
  where toProp = ActiveProp
        get o a = unsafeGet o Active >>= (\(ActiveProp v) -> return v)
instance Property Homogeneous Prop
  where toProp = HomogeneousProp
        get o a = unsafeGet o Homogeneous >>= (\(HomogeneousProp v) -> return v)
instance Property Orientation Prop
  where toProp = OrientationProp
        get o a = unsafeGet o Orientation >>= (\(OrientationProp v) -> return v)

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

