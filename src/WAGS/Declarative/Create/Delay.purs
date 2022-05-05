module WAGS.Declarative.Create.Delay where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Delay as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

delay
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialDelay i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Delay lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
delay i' atts elts = Core.Node go
  where
  Core.InitializeDelay i = Parameters.toInitializeDelay i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeDelay, setDelay }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeDelay
            { id: me, parent: parent.parent, scope: parent.scope, delayTime: i.delayTime, maxDelayTime: i.maxDelayTime }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Delay e) -> match
                  { delayTime: tmpResolveAU parent.scope di (setDelay <<< { id: me, delayTime: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

delay_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialDelay i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
delay_ i a = delay i empty a
