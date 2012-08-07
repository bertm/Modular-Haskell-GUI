{-# OPTIONS_GHC -XMultiParamTypeClasses -XExistentialQuantification #-}

-- | Operations on properties that are mostly for internal use.
-- Most of the contents of this module should therefore not be exposed to the application
-- layer, since they are of no use there.

module Graphics.UI.WebGUI.Properties.Internal (
    PropertyObject (..),
    Setter (..),
    Getter (..),
    Setting (..),
    set,
    get,
    onChange
  ) where

import Graphics.UI.WebGUI.Properties.Props
import Graphics.UI.WebGUI.Properties.Properties

-- | A PropertyObject is an object o that has property pp.
class PropertyObject o where
    unsafeSet :: Property p t => o -> p -> IO ()
    unsafeGet :: Property p t => o -> (x -> p) -> IO Prop
    unsafeOnChange :: Property p t => o -> (x -> p) -> IO () -> IO ()

-- | Indicates setting a certain property on a certain object is a valid action.
class (PropertyObject o, Property p t) => Setter o p t

-- | Indicates getting a certain property of a certain object is a valid action.
class (PropertyObject o, Property p t) => Getter o p t

data Setting o
  -- A Setter-typesafe property setter. To be used in the application.
  = forall p t x. Setter o p t => (x -> p) := x
  -- A non-Setter-typesafe property setter. Can be used for internal purposes.
  | forall p t x. (PropertyObject o, Property p t) => (x -> p) :!= x
  
-- | Sets a series of properties for an object.
set :: o -> [Setting o] -> IO ()
set obj = mapM_ (\s -> case s of
                         (p := v)  -> unsafeSet obj (p v)
                         (p :!= v) -> unsafeSet obj (p v))

-- | Gets a property of an object.
get :: Getter o p t => o -> (x -> p) -> IO t
get o a = unsafeGet o a >>= (\v -> return $ val o a $ fromProp v)
  where val :: Getter o p t => o -> (x -> p) -> p -> t
        val _ _ = toValue

onChange :: Getter o p t => o -> (x -> p) -> IO () -> IO ()
onChange = unsafeOnChange
