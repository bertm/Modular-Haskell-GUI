-- {-# OPTIONS_GHC -XRankNTypes -XMultiParamTypeClasses -XFlexibleContexts -XTypeSynonymInstances -XFlexibleInstances #-}
{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleInstances #-}

module Widgets.Internal (
    ObjectT (..),
    widgetObject,
    
    ProtoWidget (..),
    NewWidget (..)
  ) where

import Properties.Internal

-- Internal abstract representation of widgets
data ObjectT a b = O { guiObject :: b }
  deriving (Show)

-- Constructs a new internal representation of an object
widgetObject :: PropertyObject b => b -> ObjectT a b
widgetObject = O

instance PropertyObject b => PropertyObject (ObjectT a b) where
    unsafeSet (O b) = unsafeSet b
    unsafeGet (O b) = unsafeGet b

class ProtoWidget w where
  widgetClass :: w -> String

class NewWidget o t where
  new :: ProtoWidget t => [Setting t] -> o -> IO t

instance (NewWidget o a, ProtoWidget a) => NewWidget (ObjectT c o) a
  where new ss o = new ss (guiObject o)

