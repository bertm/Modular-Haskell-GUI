{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, RankNTypes #-}

-- | A wrapper module for widget-related datatypes and function.
-- The contents of this module can safely be exposed to the application layer.

module Graphics.UI.WebGUI.Widgets (
        -- * Widgets
        module Graphics.UI.WebGUI.Widgets.Widgets,
        module Graphics.UI.WebGUI.Widgets.New,
        
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

import Graphics.UI.WebGUI.Widgets.Widgets
import Graphics.UI.WebGUI.Widgets.Properties
import Graphics.UI.WebGUI.Widgets.Internal
import Graphics.UI.WebGUI.Widgets.New
import Graphics.UI.WebGUI.Types
import Graphics.UI.WebGUI.Events
import Graphics.UI.WebGUI.Actions

instance EventObject b Event => EventObject (ObjectT a b) Event where
    onEvent (O b) = onEvent b

-- All Widgets can be added to and removed from any container widget.
instance ActionAddRemove a b => ActionAddRemove (Container x a) (Widget y b) where
    add (O a) (O b) = add a b
    remove (O a) (O b) = remove a b

-- All Widgets can receive events.
instance ActionEvents a => ActionEvents (Widget x a) where
    enableEvents (O a) = enableEvents a
    disableEvents (O a) = enableEvents a

