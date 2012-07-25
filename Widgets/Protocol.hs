{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleInstances #-}
module Widgets.Protocol (
    widgetClass
  ) where

import Widgets.Widgets
import Widgets.Internal

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
