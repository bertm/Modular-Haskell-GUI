{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}

module Graphics.UI.WebGUI.Properties.Props (
    Property (..),
    Prop (..),
    mergeProp,
    sameProp
  ) where

class Property p v | p -> v where
    toValue :: p -> v
    toProp :: p -> Prop
    fromProp :: Prop -> p

data Prop

instance Show Prop

mergeProp :: Prop -> Prop -> Prop
sameProp :: Prop -> Prop -> Bool
