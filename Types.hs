{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}

module Types (GUIObject (..), IdObject (..), Setter (..), Getter (..), Property (..), EventObject (..)) where

-- | Property definitions.
class Property p pp | p -> pp where
    toProp :: p -> pp
    get :: Getter o pp p => o -> (x -> p) -> IO p

-- | A GUIObject is an object o that has property pp.
class GUIObject o pp | o -> pp where
    unsafeSet :: Property p pp => o -> p -> IO ()
    unsafeGet :: Property p pp => o -> (x -> p) -> IO pp

-- | An IdObject is an object o that has an integer identifier.
class IdObject o where
    getIdentifier :: o -> Integer

-- | An EventObject is an object o that can have a certain Event e.
class EventObject o e | o -> e where
    on :: o -> e -> (e -> IO ()) -> IO ()

-- | Indicates setting a certain property on a certain object is a valid action.
class (GUIObject o pp, Property p pp) => Setter o pp p where
    set :: o -> (x -> p) -> x -> IO ()
    set o f p = unsafeSet o (f p)

-- | Indicates getting a certain property of a certain object is a valid action.
class (GUIObject o pp, Property p pp) => Getter o pp p

