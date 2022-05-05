module WAGS.Declarative.Create.Highpass where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Highpass as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

highpass
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialHighpass i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Highpass lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
highpass i' atts elts = Core.Node go
  where
  Core.InitializeHighpass i = Parameters.toInitializeHighpass i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeHighpass, setFrequency, setQ }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeHighpass
            { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Highpass e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

highpass_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialHighpass i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
highpass_ i a = highpass i empty a
