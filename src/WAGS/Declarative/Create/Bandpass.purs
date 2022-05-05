module WAGS.Declarative.Create where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Bandpass as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

bandpass
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialBandpass i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Bandpass lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
bandpass i' atts elts = Core.Node go
  where
  Core.InitializeBandpass i = Parameters.toInitializeBandpass i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeBandpass, setFrequency, setQ }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeBandpass
            { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Bandpass e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

bandpass_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialBandpass i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
bandpass_ i a = bandpass i empty a
