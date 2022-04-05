module WAGS.Properties where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))
import WAGS.Parameter (class ToAudioParameter, AudioParameter, toAudioParameter)

-- props

frequency
  :: forall nt r ap
   . Newtype nt (Variant (frequency :: AudioParameter | r))
  => ToAudioParameter ap
  => ap
  -> nt
frequency = wrap <<< inj (Proxy :: Proxy "frequency") <<< toAudioParameter