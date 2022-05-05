module WAGS.Declarative.Create.Notch where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Notch as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

notch
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialNotch i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Notch lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
notch i' atts elts = Core.Node go
  where
  Core.InitializeNotch i = Parameters.toInitializeNotch i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeNotch, setFrequency, setQ }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeNotch
            { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Notch e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

notch_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialNotch i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
notch_ i a = notch i empty a
