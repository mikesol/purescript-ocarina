module WAGS.Declarative.Create.Lowpass where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Lowpass as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

lowpass
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialLowpass i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Lowpass lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
lowpass i' atts elts = Core.Node go
  where
  Core.InitializeLowpass i = Parameters.toInitializeLowpass i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeLowpass, setFrequency, setQ }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeLowpass
            { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Lowpass e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

lowpass_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialLowpass i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
lowpass_ i a = lowpass i empty a
