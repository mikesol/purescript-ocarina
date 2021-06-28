module WAGS.Graph.Paramable where

import Prelude

import WAGS.Graph.AudioUnit (APOnOff, OnOff)
import WAGS.Graph.Parameter (AudioParameter)

-- | A value that can be coerced to an initial control-rate audio parameter.
class Paramable a where
  paramize :: a -> AudioParameter

instance paramableNumber :: Paramable Number where
  paramize = pure

instance paramableAudioParameter :: Paramable AudioParameter where
  paramize = identity

class OnOffable a where
  onOffIze :: a -> APOnOff

instance onOffIzeOnOff :: OnOffable OnOff where
  onOffIze = pure

instance onOffIzeAudioParameter :: OnOffable APOnOff where
  onOffIze = identity
