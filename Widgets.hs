{-# OPTIONS_GHC -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleContexts -XRankNTypes #-}

-- | A wrapper module for widget-related datatypes and function.
-- The contents of this module can safely be exposed to the application layer.

module Widgets (
        -- * Widgets
        module Widgets.Widgets,
        module Widgets.New,
        
        -- * Actions
        add,
        remove,
        showSingle,
        showAll,
        hideSingle,
        hideAll,
        enableEvents,
        disableEvents,
    ) where

import Widgets.Widgets
import Widgets.Properties
import Widgets.Internal
import Widgets.New
import Types
import Events
import Actions

instance EventObject b Event => EventObject (ObjectT a b) Event where
    on (O b) = on b


-- Widgets
instance ActionAddRemove a b => ActionAddRemove (Widget x a) (Widget y b) where
    add (O a) (O b) = add a b
    remove (O a) (O b) = remove a b
instance ActionEvents a => ActionEvents (Widget x a) where
    enableEvents (O a) = enableEvents a
    disableEvents (O a) = enableEvents a




