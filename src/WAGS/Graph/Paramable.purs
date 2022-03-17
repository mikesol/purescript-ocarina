module WAGS.Graph.Paramable where

import Prelude

import WAGS.Graph.Parameter (AudioEnvelope, AudioOnOff(..), AudioParameter, AudioParameterCancellation, AudioSingleNumber(..), OnOff, _linearRamp, cancellation, envelope, singleNumber)

-- | A value that can be coerced to an initial control-rate audio parameter.
class Paramable a where
  paramize :: a -> AudioParameter

instance paramableNumber :: Paramable Number where
  paramize = singleNumber <<< AudioSingleNumber <<<
    { timeOffset: 0.0, transition: _linearRamp, param: _ }

instance paramableSingleNumber :: Paramable AudioSingleNumber where
  paramize = singleNumber

instance paramableCancellation :: Paramable AudioParameterCancellation where
  paramize = cancellation

instance paramableEnvelope :: Paramable AudioEnvelope where
  paramize = envelope

instance paramableAudioParameter :: Paramable AudioParameter where
  paramize = identity

class OnOffable a where
  onOffIze :: a -> AudioOnOff

instance onOffIzeOnOff :: OnOffable OnOff where
  onOffIze = AudioOnOff <<< { timeOffset: 0.0, onOff: _ }

instance onOffIzeAudioParameter :: OnOffable AudioOnOff where
  onOffIze = identity
