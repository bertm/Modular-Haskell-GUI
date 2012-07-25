module Graphics.UI.WebGUI.Properties.Props (
    Property (..),
    Prop (..),
    mergeProp,
    sameProp
  ) where

class Property p where
    toProp :: p -> Prop
    fromProp :: Prop -> p

data Prop

instance Show Prop

mergeProp :: Prop -> Prop -> Prop
sameProp :: Prop -> Prop -> Bool
