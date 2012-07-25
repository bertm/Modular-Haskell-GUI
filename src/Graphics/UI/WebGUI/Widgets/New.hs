{-# OPTIONS_GHC -XFlexibleContexts -XRankNTypes #-}

-- | This module defines ways of creating a new widget in the GUI.
-- This module is exported by the Widgets module, and does therefore usually not have
-- to be imported directly.

module Graphics.UI.WebGUI.Widgets.New (
    newWindow,
    newButton,
    newLineEdit,
    newBox,
    newMainWindow
  ) where

import Graphics.UI.WebGUI.Widgets.Widgets
import Graphics.UI.WebGUI.Widgets.Properties
import Graphics.UI.WebGUI.Widgets.Internal

import Graphics.UI.WebGUI.Properties.Internal

-- A helper type to make the widget constructors more elegant.
type New obj = (ProtoWidget (obj () o),
                NewWidget o (obj () o),
                PropertyObject o) =>
                  Screen () o -> IO (obj () o)

-- | Creates a new Window widget.
newWindow :: New Window
newWindow = new windowDefaults

-- | Creates a new Button widget.
newButton :: New Button
newButton = new buttonDefaults

-- | Creates a new LineEdit widget.
newLineEdit :: New LineEdit
newLineEdit = new lineEditDefaults

-- | Creates a new Box widget.
newBox :: New Box
newBox = new boxDefaults

-- | Creates a new MainWindow Widget.
newMainWindow :: New MainWindow
newMainWindow = new mainWindowDefaults

