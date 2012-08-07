{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Graphics.UI.WebGUI.Widgets.Protocol (
    widgetClass
  ) where

import Graphics.UI.WebGUI.Widgets.Widgets
import Graphics.UI.WebGUI.Widgets.Internal

instance ProtoWidget (Window () o) where
    widgetClass _ = "Window"
instance ProtoWidget (Button () o) where
    widgetClass _ = "Button"
instance ProtoWidget (LineEdit () o) where
  widgetClass _ = "LineEdit"
instance ProtoWidget (Box () o) where
  widgetClass _ = "Box"
instance ProtoWidget (MainWindow () o) where
  widgetClass _ = "MainWindow"
