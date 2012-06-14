{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleContexts -XRankNTypes #-}

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
        LineEdit,
        lineEditDefaults,
        Box,
        boxDefaults,
        MainWindow,
        mainWindowDefaults,
        
        -- * Actions
        add,
        remove,
        showSingle,
        showAll,
        hideSingle,
        hideAll,
        enableEvents,
        disableEvents,
        
        -- * Object manipulation
        newObject,
        obj,
        set
    ) where

import Types
import Properties
import Actions

type Defaults obj = GUIObject b Prop => [Setting (obj a b)]-- GUIObject b Prop => obj a b -> [IO ()]

-- Constructs a new internal representation of an object
newObject :: GUIObject b pp => b -> ObjectT a b
newObject = O

-- Internal abstract representation of widgets
data ObjectT a b = O { obj :: b }
  deriving (Show)
instance GUIObject b Prop => GUIObject (ObjectT a b) Prop where
    unsafeSet (O b) = unsafeSet b
    unsafeGet (O b) = unsafeGet b
instance IdObject b => IdObject (ObjectT a b) where
    getIdentifier (O b) = getIdentifier b
instance EventObject b Event => EventObject (ObjectT a b) Event where
    on (O b) = on b

-- | Sets a series of properties for an object.
set :: o -> [Setting o] -> IO ()
set obj = mapM_ (\s -> case s of
                         (p := v)  -> unsafeSet obj (p v)
                         (p :!= v) -> unsafeSet obj (p v))

-- Widgets
type Screen a b = ObjectT (AbstractScreen a) b
data AbstractScreen a = AbstractScreen

type Widget a b = ObjectT (AbstractWidget a) b
data AbstractWidget a = AbstractWidget
instance GUIObject b Prop => Setter (Widget a b) Prop Visible
instance GUIObject b Prop => Setter (Widget a b) Prop Size
instance GUIObject b Prop => Setter (Widget a b) Prop Margin
instance GUIObject b Prop => Setter (Widget a b) Prop Sensitive
instance GUIObject b Prop => Setter (Widget a b) Prop CanFocus
instance GUIObject b Prop => Setter (Widget a b) Prop Events -- TODO: remove?
instance GUIObject b Prop => Getter (Widget a b) Prop Active -- Maybe only for Window? TODO: check
instance ActionAddRemove a b => ActionAddRemove (Widget x a) (Widget y b) where
    add (O a) (O b) = add a b
    remove (O a) (O b) = remove a b
instance ActionEvents a => ActionEvents (Widget x a) where
    enableEvents (O a) = enableEvents a
    disableEvents (O a) = enableEvents a
widgetDefaults :: Defaults Widget
widgetDefaults = [Visible := False,
                  Sensitive := True,
                  CanFocus := False,
                  Events := [],
                  Active :!= False]

type Container a b = Widget (AbstractContainer a) b
data AbstractContainer a = AbstractContainer
containerDefaults :: Defaults Container
containerDefaults = widgetDefaults

type Box a b = Container (AbstractBox a) b
data AbstractBox a = AbstractBox
instance GUIObject b Prop => Setter (Box a b) Prop Homogeneous
instance GUIObject b Prop => Setter (Box a b) Prop Orientation
boxDefaults :: Defaults Box
boxDefaults = [Homogeneous := True,
               Orientation := "horizontal"]
              ++ containerDefaults

type Bin a b = Container (AbstractBin a) b
data AbstractBin a = AbstractBin
binDefaults :: Defaults Bin
binDefaults = containerDefaults

type Button a b = Bin (AbstractButton a) b
data AbstractButton a = AbstractButton
instance GUIObject b Prop => Setter (Button a b) Prop Label
instance GUIObject b Prop => Getter (Button a b) Prop Label
buttonDefaults :: Defaults Button
buttonDefaults = [Label := ""]
                 ++ binDefaults

type Window a b = Bin (AbstractWindow a) b
data AbstractWindow a = AbstractWindow
instance GUIObject b Prop => Setter (Window a b) Prop Title
instance GUIObject b Prop => Setter (Window a b) Prop Opacity
windowDefaults :: Defaults Window
windowDefaults = [Title := "",
                  Opacity := 1]
                 ++ binDefaults

type MainWindow a b = Bin (AbstractMainWindow a) b
data AbstractMainWindow a = AbstractMainWindow
instance GUIObject b Prop => Setter (MainWindow a b) Prop Title
mainWindowDefaults :: Defaults MainWindow
mainWindowDefaults = [Title := ""]
                     ++ binDefaults

type LineEdit a b = Widget (AbstractLineEdit a) b
data AbstractLineEdit a = AbstractLineEdit
instance GUIObject b Prop => Setter (LineEdit a b) Prop Text
instance GUIObject b Prop => Setter (LineEdit a b) Prop Editable
instance GUIObject b Prop => Setter (LineEdit a b) Prop Visibility
instance GUIObject b Prop => Setter (LineEdit a b) Prop MaxLength
instance GUIObject b Prop => Getter (LineEdit a b) Prop Text
lineEditDefaults :: Defaults LineEdit
lineEditDefaults = [Text := "",
                    Editable := True,
                    Visibility := True,
                    MaxLength := 0]
                   ++ widgetDefaults

