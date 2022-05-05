module WAGS.Declarative.Create.Speaker where

import Prelude

import Data.Variant.Maybe (just)
import Data.Typelevel.Num (D2)
import FRP.Event (Event, makeEvent, subscribe)
import WAGS.Core as Core

speaker
  :: forall aud (outputChannels :: Type) lock payload
   . Core.Mix aud (Core.Audible outputChannels lock payload)
  => aud
  -> Core.AudioInterpret payload
  -> Event payload
speaker elts di@(Core.AudioInterpret { ids, makeSpeaker }) = makeEvent \k -> do
  id <- ids
  k (makeSpeaker { id })
  subscribe (Core.__internalWagsFlatten (just id) "toplevel" di (Core.mix elts)) k

speaker2
  :: forall aud lock payload
   . Core.Mix aud (Core.Audible D2 lock payload)
  => aud
  -> Core.AudioInterpret payload
  -> Event payload
speaker2 = speaker
