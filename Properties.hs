-- | Definition of all possible properties widgets may or may not have.

module Properties (
    module Properties.Properties,
    
    Setting ((:=)),
    get,
    set
  ) where

import Properties.Internal
import Properties.Properties
