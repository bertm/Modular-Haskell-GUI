{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}

module Types (GUIObject (..), IdObject (..), Setter (..), Getter (..), Property (..)) where

class Property p pp | p -> pp where
    toProp :: p -> pp
    get :: Getter o pp p => o -> (a -> p) -> IO p

class GUIObject o pp | o -> pp where
    setProperty :: Property p pp => o -> p -> IO ()
    getProperty :: Property p pp => o -> (a -> p) -> IO pp
    addChildObject :: (GUIObject oo pp, IdObject oo) => o -> oo -> IO ()

class IdObject o where
    getIdentifier :: o -> Int

class (GUIObject o pp, Property p pp) => Setter o pp p where
    set :: o -> (a -> p) -> a -> IO ()
    set o f p = setProperty o (f p)

class (GUIObject o pp, Property p pp) => Getter o pp p

