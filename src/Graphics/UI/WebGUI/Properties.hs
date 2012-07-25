-- | Definition of all possible properties widgets may or may not have.

module Graphics.UI.WebGUI.Properties (
    module Graphics.UI.WebGUI.Properties.Properties,
    
    Setting ((:=)),
    get,
    set
  ) where

import Graphics.UI.WebGUI.Properties.Internal
import Graphics.UI.WebGUI.Properties.Properties
