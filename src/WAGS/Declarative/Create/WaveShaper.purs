module WAGS.Declarative.Create.WaveShaper where

import Prelude

import Control.Alt ((<|>))
import Data.Variant.Maybe (just)
import FRP.Event (bang, makeEvent, subscribe)
import WAGS.Common.Parameters.WaveShaper as Parameters
import WAGS.Core as Core

waveShaper
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialWaveShaper i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
waveShaper i' elts = Core.Node go
  where
  Core.InitializeWaveShaper i = Parameters.toInitializeWaveShaper i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeWaveShaper }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeWaveShaper
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , curve: i.curve
              , oversample: i.oversample
              }
          ) <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)
