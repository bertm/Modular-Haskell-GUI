module Widgets.Widgets (
    Screen,
    Widget,
    Container,
    Bin,
    Button,
    Window,
    LineEdit,
    Box,
    MainWindow
  ) where
  
import Widgets.Internal
  
type Screen a b = ObjectT (AbstractScreen a) b
data AbstractScreen a = AbstractScreen

type Widget a b = ObjectT (AbstractWidget a) b
data AbstractWidget a = AbstractWidget

type Container a b = Widget (AbstractContainer a) b
data AbstractContainer a = AbstractContainer

type Box a b = Container (AbstractBox a) b
data AbstractBox a = AbstractBox

type Bin a b = Container (AbstractBin a) b
data AbstractBin a = AbstractBin

type Button a b = Bin (AbstractButton a) b
data AbstractButton a = AbstractButton

type Window a b = Bin (AbstractWindow a) b
data AbstractWindow a = AbstractWindow

type MainWindow a b = Bin (AbstractMainWindow a) b
data AbstractMainWindow a = AbstractMainWindow

type LineEdit a b = Widget (AbstractLineEdit a) b
data AbstractLineEdit a = AbstractLineEdit
