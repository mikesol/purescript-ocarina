module Ocarina.Example.Utils where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, Event, create, makeEvent, subscribe)
import Ocarina.WebAPI (AudioContext)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

type ToCancel = { unsub :: Effect Unit, ctx :: AudioContext }
type RaiseCancellation = Maybe ToCancel -> Effect Unit
