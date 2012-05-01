{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}

module Types (GUIObject (..), IdObject (..), Setter (..), Getter (..), Property (..), EventObject (..)) where

class Property p pp | p -> pp where
    toProp :: p -> pp
    get :: Getter o pp p => o -> (x -> p) -> IO p

class GUIObject o pp | o -> pp where
    setProperty :: Property p pp => o -> p -> IO ()
    getProperty :: Property p pp => o -> (x -> p) -> IO pp
    addChildObject :: (GUIObject oo pp, IdObject oo) => o -> oo -> IO ()

class IdObject o where
    getIdentifier :: o -> Integer

class EventObject o e | o -> e where
    on :: o -> e -> (e -> IO ()) -> IO ()

class (GUIObject o pp, Property p pp) => Setter o pp p where
    set :: o -> (x -> p) -> x -> IO ()
    set o f p = setProperty o (f p)

class (GUIObject o pp, Property p pp) => Getter o pp p

{- class (GUIObject o pp a, 

class Action a aa | a -> aa where
    toAct :: a -> aa
    perform :: Performable o aa a => o -> (x -> a) -> IO () -}
