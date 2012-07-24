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
        obj
    ) where

import Types
import Properties.Internal
import Actions

type Defaults obj = PropertyObject b => [Setting (obj a b)]

-- Constructs a new internal representation of an object
newObject :: PropertyObject b => b -> ObjectT a b
newObject = O

-- Internal abstract representation of widgets
data ObjectT a b = O { obj :: b }
  deriving (Show)

instance PropertyObject b => PropertyObject (ObjectT a b) where
    unsafeSet (O b) = unsafeSet b
    unsafeGet (O b) = unsafeGet b

instance EventObject b Event => EventObject (ObjectT a b) Event where
    on (O b) = on b


-- Widgets
type Screen a b = ObjectT (AbstractScreen a) b
data AbstractScreen a = AbstractScreen

type Widget a b = ObjectT (AbstractWidget a) b
data AbstractWidget a = AbstractWidget
instance PropertyObject b => Setter (Widget a b) Visible
instance PropertyObject b => Setter (Widget a b) Size
instance PropertyObject b => Setter (Widget a b) Margin
instance PropertyObject b => Setter (Widget a b) Sensitive
instance PropertyObject b => Setter (Widget a b) CanFocus
instance PropertyObject b => Setter (Widget a b) Events -- TODO: remove?
instance PropertyObject b => Getter (Widget a b) Active -- Maybe only for Window? TODO: check
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
instance PropertyObject b => Setter (Box a b) Homogeneous
instance PropertyObject b => Setter (Box a b) Orientation
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
instance PropertyObject b => Setter (Button a b) Label
instance PropertyObject b => Getter (Button a b) Label
buttonDefaults :: Defaults Button
buttonDefaults = [Label := ""]
                 ++ binDefaults

type Window a b = Bin (AbstractWindow a) b
data AbstractWindow a = AbstractWindow
instance PropertyObject b => Setter (Window a b) Title
instance PropertyObject b => Setter (Window a b) Opacity
windowDefaults :: Defaults Window
windowDefaults = [Title := "",
                  Opacity := 1]
                 ++ binDefaults

type MainWindow a b = Bin (AbstractMainWindow a) b
data AbstractMainWindow a = AbstractMainWindow
instance PropertyObject b => Setter (MainWindow a b) Title
mainWindowDefaults :: Defaults MainWindow
mainWindowDefaults = [Title := ""]
                     ++ binDefaults

type LineEdit a b = Widget (AbstractLineEdit a) b
data AbstractLineEdit a = AbstractLineEdit
instance PropertyObject b => Setter (LineEdit a b) Text
instance PropertyObject b => Setter (LineEdit a b) Editable
instance PropertyObject b => Setter (LineEdit a b) Visibility
instance PropertyObject b => Setter (LineEdit a b) MaxLength
instance PropertyObject b => Getter (LineEdit a b) Text
lineEditDefaults :: Defaults LineEdit
lineEditDefaults = [Text := "",
                    Editable := True,
                    Visibility := True,
                    MaxLength := 0]
                   ++ widgetDefaults

