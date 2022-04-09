module FRP.Event.Memoize where

import Prelude

import FRP.Event (Event)

foreign import memoize :: Event ~> Event