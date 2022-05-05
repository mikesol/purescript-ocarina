module WAGS.Declarative.Create.Peaking where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Peaking as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

-- peaking
peaking
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialPeaking i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Peaking lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
peaking i' atts elts = Core.Node go
  where
  Core.InitializePeaking i = Parameters.toInitializePeaking i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makePeaking, setFrequency, setQ, setGain }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makePeaking
            { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q, gain: i.gain }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Peaking e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

peaking_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialPeaking i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
peaking_ i a = peaking i empty a
