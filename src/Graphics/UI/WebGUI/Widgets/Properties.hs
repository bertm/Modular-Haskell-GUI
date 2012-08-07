{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, RankNTypes, FlexibleInstances #-}

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
import Graphics.UI.WebGUI.Properties.Types
import Graphics.UI.WebGUI.Properties.Props

type Defaults obj = PropertyObject b => [Setting (obj a b)]

-- * Widget
instance (PropertyObject b, Property Visible t) => Setter (Widget a b) Visible t
instance (PropertyObject b, Property Size t) => Setter (Widget a b) Size t
instance (PropertyObject b, Property Margin t) => Setter (Widget a b) Margin t
instance (PropertyObject b, Property Sensitive t) => Setter (Widget a b) Sensitive t
instance (PropertyObject b, Property CanFocus t) => Setter (Widget a b) CanFocus t
instance (PropertyObject b, Property Events t) => Setter (Widget a b) Events t -- TODO: remove?
instance (PropertyObject b, Property Active t) => Getter (Widget a b) Active t -- Maybe only for Window? TODO: check
widgetDefaults :: Defaults Widget
widgetDefaults = [Visible := False,
                  Sensitive := True,
                  CanFocus := False,
                  Events := [],
                  Active :!= False,
                  Parent :!= Nothing]

-- * Container
containerDefaults :: Defaults Container
containerDefaults = widgetDefaults

-- * Box
instance (PropertyObject b, Property Homogeneous t) => Setter (Box a b) Homogeneous t
instance (PropertyObject b, Property Orientation t) => Setter (Box a b) Orientation t
boxDefaults :: Defaults Box
boxDefaults = [Homogeneous := True,
               Orientation := HorizontalOrientation]
              ++ containerDefaults

-- * Bin
binDefaults :: Defaults Bin
binDefaults = containerDefaults

-- * Button
instance (PropertyObject b, Property Label t) => Setter (Button a b) Label t
instance (PropertyObject b, Property Label t) => Getter (Button a b) Label t
buttonDefaults :: Defaults Button
buttonDefaults = [Label := ""]
                 ++ binDefaults

-- * Window
instance (PropertyObject b, Property Title t) => Setter (Window a b) Title t
instance (PropertyObject b, Property Opacity t) => Setter (Window a b) Opacity t
windowDefaults :: Defaults Window
windowDefaults = [Title := "",
                  Opacity := 1]
                 ++ binDefaults

-- * MainWindow
instance (PropertyObject b, Property Title t) => Setter (MainWindow a b) Title t
mainWindowDefaults :: Defaults MainWindow
mainWindowDefaults = [Title := ""]
                     ++ binDefaults

-- * LineEdit
instance (PropertyObject b, Property Text t) => Setter (LineEdit a b) Text t
instance (PropertyObject b, Property Editable t) => Setter (LineEdit a b) Editable t
instance (PropertyObject b, Property Visibility t) => Setter (LineEdit a b) Visibility t
instance (PropertyObject b, Property MaxLength t) => Setter (LineEdit a b) MaxLength t
instance (PropertyObject b, Property Text t) => Getter (LineEdit a b) Text t
lineEditDefaults :: Defaults LineEdit
lineEditDefaults = [Text := "",
                    Editable := True,
                    Visibility := True,
                    MaxLength := 0]
                   ++ widgetDefaults

