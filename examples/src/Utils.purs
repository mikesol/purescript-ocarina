module Ocarina.Example.Utils where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Ocarina.WebAPI (AudioContext)

type ToCancel = { unsub :: Effect Unit, ctx :: AudioContext }
type RaiseCancellation = Maybe ToCancel -> Effect Unit