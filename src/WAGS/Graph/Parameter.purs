module WAGS.Graph.Parameter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data AudioParameterTransition
  = NoRamp
  | LinearRamp
  | ExponentialRamp
  | Immediately

derive instance eqAudioParameterTransition :: Eq AudioParameterTransition

derive instance genericAudioParameterTransition :: Generic AudioParameterTransition _

instance showAudioParameterTransition :: Show AudioParameterTransition where
  show = genericShow

type AudioParameter'
  = { param :: Number
    , timeOffset :: Number
    , transition :: AudioParameterTransition
    , forceSet :: Boolean
    }

newtype AudioParameter
  = AudioParameter AudioParameter'

param :: Number -> AudioParameter
param =
  AudioParameter
    <<< defaultParam
        { param = _
        }

derive newtype instance eqAudioParameter :: Eq AudioParameter

derive newtype instance showAudioParameter :: Show AudioParameter


defaultParam :: AudioParameter'
defaultParam = { param: 0.0, timeOffset: 0.0, transition: LinearRamp, forceSet: false }
