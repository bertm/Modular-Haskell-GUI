-- | Definition of all possible properties widgets may or may not have.

module Properties (
    Visible (..),
    Size (..),
    Margin (..),
    Sensitive (..),
    CanFocus (..),
    Title (..),
    Opacity (..),
    Label (..),
    Parent (..),
    Text (..),
    Editable (..),
    Visibility (..),
    MaxLength (..),
    Events (..),
    Event (..), -- TODO: Do these both have to be exported?
    Active (..),
    Homogeneous (..),
    Orientation (..),
    
    Property (get),
    Setting ((:=)),
    set
  ) where

import Properties.Internal
