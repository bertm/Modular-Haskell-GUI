{-# OPTIONS_GHC -XMultiParamTypeClasses -XExistentialQuantification #-}

module Properties.Internal (
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
    
    PropertyObject (..),
    Setter (..),
    Getter (..),
    Property (..),
    Setting (..),
    set,
    get,
    
    Prop (..),
    sameProp,
    mergeProp
  ) where

-- | Property definitions.
class Property p where
    toProp :: p -> Prop
    fromProp :: Prop -> p

-- | A PropertyObject is an object o that has property pp.
class PropertyObject o where
    unsafeSet :: Property p => o -> p -> IO ()
    unsafeGet :: Property p => o -> (x -> p) -> IO Prop

-- | Indicates setting a certain property on a certain object is a valid action.
class (PropertyObject o, Property p) => Setter o p

-- | Indicates getting a certain property of a certain object is a valid action.
class (PropertyObject o, Property p) => Getter o p

data Setting o
  -- A Setter-typesafe property setter. To be used in the application.
  = forall p x. Setter o p => (x -> p) := x
  -- A non-Setter-typesafe property setter. Can be used for internal purposes.
  | forall p x. (PropertyObject o, Property p) => (x -> p) :!= x
  
-- | Sets a series of properties for an object.
set :: o -> [Setting o] -> IO ()
set obj = mapM_ (\s -> case s of
                         (p := v)  -> unsafeSet obj (p v)
                         (p :!= v) -> unsafeSet obj (p v))

-- | Gets a property of an object.
get :: Getter o p => o -> (x -> p) -> IO p
get o a = unsafeGet o a >>= (\v -> return (fromProp v))

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
           | forall a p. Property p => Change (a -> p)
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

