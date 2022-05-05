module WAGS.Declarative.Create.StereoPanner where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.StereoPanner as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

pan
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialStereoPanner i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.StereoPanner lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
pan i' atts elts = Core.Node go
  where
  Core.InitializeStereoPanner i = Parameters.toInitializeStereoPanner i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeStereoPanner, setPan }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeStereoPanner
            { id: me, parent: parent.parent, scope: parent.scope, pan: i.pan }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.StereoPanner e) -> match
                  { pan: tmpResolveAU parent.scope di (setPan <<< { id: me, pan: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

pan_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialStereoPanner i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
pan_ i = pan i empty
