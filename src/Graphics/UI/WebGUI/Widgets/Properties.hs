{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleContexts -XRankNTypes #-}

-- | This module defines for each widget which properties can be get and set, and what
-- the default property values are.

module Graphics.UI.WebGUI.Widgets.Properties (
    widgetDefaults,
    containerDefaults,
    binDefaults,
    buttonDefaults,
    windowDefaults,
    lineEditDefaults,
    boxDefaults,
    mainWindowDefaults
  ) where

import Graphics.UI.WebGUI.Widgets.Widgets
import Graphics.UI.WebGUI.Widgets.Internal
import Graphics.UI.WebGUI.Properties
import Graphics.UI.WebGUI.Properties.Internal

type Defaults obj = PropertyObject b => [Setting (obj a b)]

-- * Widget
instance PropertyObject b => Setter (Widget a b) Visible
instance PropertyObject b => Setter (Widget a b) Size
instance PropertyObject b => Setter (Widget a b) Margin
instance PropertyObject b => Setter (Widget a b) Sensitive
instance PropertyObject b => Setter (Widget a b) CanFocus
instance PropertyObject b => Setter (Widget a b) Events -- TODO: remove?
instance PropertyObject b => Getter (Widget a b) Active -- Maybe only for Window? TODO: check
widgetDefaults :: Defaults Widget
widgetDefaults = [Visible := False,
                  Sensitive := True,
                  CanFocus := False,
                  Events := [],
                  Active :!= False]

-- * Container
containerDefaults :: Defaults Container
containerDefaults = widgetDefaults

-- * Box
instance PropertyObject b => Setter (Box a b) Homogeneous
instance PropertyObject b => Setter (Box a b) Orientation
boxDefaults :: Defaults Box
boxDefaults = [Homogeneous := True,
               Orientation := "horizontal"]
              ++ containerDefaults

-- * Bin
binDefaults :: Defaults Bin
binDefaults = containerDefaults

-- * Button
instance PropertyObject b => Setter (Button a b) Label
instance PropertyObject b => Getter (Button a b) Label
buttonDefaults :: Defaults Button
buttonDefaults = [Label := ""]
                 ++ binDefaults

-- * Window
instance PropertyObject b => Setter (Window a b) Title
instance PropertyObject b => Setter (Window a b) Opacity
windowDefaults :: Defaults Window
windowDefaults = [Title := "",
                  Opacity := 1]
                 ++ binDefaults

-- * MainWindow
instance PropertyObject b => Setter (MainWindow a b) Title
mainWindowDefaults :: Defaults MainWindow
mainWindowDefaults = [Title := ""]
                     ++ binDefaults

-- * LineEdit
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

