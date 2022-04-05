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

loopEnd
  :: forall nt r
   . Newtype nt (Variant (loopEnd :: Number | r))
  => Number
  -> nt
loopEnd = wrap <<< inj (Proxy :: Proxy "loopEnd")

loopStart
  :: forall nt r
   . Newtype nt (Variant (loopStart :: Number | r))
  => Number
  -> nt
loopStart = wrap <<< inj (Proxy :: Proxy "loopStart")

playbackRate
  :: forall nt r ap
   . Newtype nt (Variant (playbackRate :: AudioParameter | r))
  => ToAudioParameter ap
  => ap
  -> nt
playbackRate = wrap <<< inj (Proxy :: Proxy "playbackRate") <<< toAudioParameter