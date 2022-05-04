module WAGS.Declarative.Create.Highshelf where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Highshelf as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

highshelf
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialHighshelf i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Highshelf lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
highshelf i' atts elts = Core.Node go
  where
  Core.InitializeHighshelf i = Parameters.toInitializeHighshelf i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeHighshelf, setFrequency, setGain }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeHighshelf
            { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, gain: i.gain }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Highshelf e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

highshelf_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialHighshelf i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
highshelf_ i a = highshelf i empty a
