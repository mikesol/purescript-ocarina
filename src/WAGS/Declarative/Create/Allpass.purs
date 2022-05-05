module WAGS.Declarative.Create.Allpass where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Allpass as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

allpass
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialAllpass i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Allpass lock payload)
  -> aud -- Array (Core.Node outputChannels lock payload)
  -> Core.Node outputChannels lock payload
allpass i' atts elts = Core.Node go
  where
  Core.InitializeAllpass i = Parameters.toInitializeAllpass i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeAllpass, setFrequency, setQ }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeAllpass
            { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Allpass e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

allpass_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialAllpass i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
allpass_ i a = allpass i empty a
