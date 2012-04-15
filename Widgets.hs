{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleContexts #-}

-- | Definition of the widgets of the GUI.

module Widgets (
        -- * Widgets
        Screen,
        Widget,
        widgetDefaults,
        Container,
        containerDefaults,
        Bin,
        binDefaults,
        Button,
        buttonDefaults,
        Window,
        windowDefaults,
        Entry,
        entryDefaults,
        
        -- * Object manipulation
        newObject,
        obj
    ) where

import Types
import Properties

-- Constructs a new internal representation of an object
newObject :: GUIObject b pp => b -> ObjectT a b
newObject = O

-- Internal abstract representation of widgets
data ObjectT a b = O { obj :: b }
  deriving (Show)
instance GUIObject b Prop => GUIObject (ObjectT a b) Prop where
    setProperty (O b) = setProperty b
    getProperty (O b) = getProperty b
    addChildObject (O b) = addChildObject b
instance IdObject b => IdObject (ObjectT a b) where
    getIdentifier (O b) = getIdentifier b
instance EventObject b Event => EventObject (ObjectT a b) Event where
    on (O b) = on b

-- Widgets
type Screen a b = ObjectT (AbstractScreen a) b
data AbstractScreen a = AbstractScreen

type Widget a b = ObjectT (AbstractWidget a) b
data AbstractWidget a = AbstractWidget
instance GUIObject b Prop => Setter (Widget a b) Prop Visible
instance GUIObject b Prop => Setter (Widget a b) Prop Size
instance GUIObject b Prop => Setter (Widget a b) Prop Margin
instance GUIObject b Prop => Setter (Widget a b) Prop Sensitive
instance GUIObject b Prop => Setter (Widget a b) Prop Focus
instance GUIObject b Prop => Setter (Widget a b) Prop Events
widgetDefaults = [VisibleProp (Visible False),
                  SizeProp (Size (100, 100)),
                  MarginProp (Margin (0, 0, 0, 0)),
                  SensitiveProp (Sensitive False),
                  FocusProp (Focus False),
                  EventsProp (Events [])]

type Container a b = Widget (AbstractContainer a) b
data AbstractContainer a = AbstractContainer
containerDefaults = widgetDefaults

type Bin a b = Container (AbstractBin a) b
data AbstractBin a = AbstractBin
binDefaults = containerDefaults

type Button a b = Bin (AbstractButton a) b
data AbstractButton a = AbstractButton
instance GUIObject b Prop => Setter (Button a b) Prop Label
instance GUIObject b Prop => Getter (Button a b) Prop Label
buttonDefaults = LabelProp (Label "") : binDefaults

type Window a b = Bin (AbstractWindow a) b
data AbstractWindow a = AbstractWindow
instance GUIObject b Prop => Setter (Window a b) Prop Title
instance GUIObject b Prop => Setter (Window a b) Prop Opacity
windowDefaults = TitleProp (Title "") : OpacityProp (Opacity 1) : binDefaults

type Entry a b = Widget (AbstractEntry a) b
data AbstractEntry a = AbstractEntry
instance GUIObject b Prop => Setter (Entry a b) Prop Text
instance GUIObject b Prop => Setter (Entry a b) Prop Editable
instance GUIObject b Prop => Setter (Entry a b) Prop Visibility
instance GUIObject b Prop => Setter (Entry a b) Prop MaxLength
instance GUIObject b Prop => Getter (Entry a b) Prop Text
entryDefaults = TextProp (Text "") : EditableProp (Editable True) : VisibilityProp (Visibility True) : MaxLengthProp (MaxLength 0) : widgetDefaults

