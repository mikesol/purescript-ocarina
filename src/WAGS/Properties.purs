module WAGS.Properties where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))
import WAGS.Parameter (class ToAudioOnOff, class ToAudioParameter, AudioOnOff, AudioParameter, toAudioOnOff, toAudioParameter)
import WAGS.WebAPI (BrowserAudioBuffer)

-- props

buffer
  :: forall nt r
   . Newtype nt (Variant (buffer :: BrowserAudioBuffer | r))
  => BrowserAudioBuffer
  -> nt
buffer = wrap <<< inj (Proxy :: Proxy "buffer")

bufferOffset
  :: forall nt r
   . Newtype nt (Variant (bufferOffset :: Number | r))
  => Number
  -> nt
bufferOffset = wrap <<< inj (Proxy :: Proxy "bufferOffset")

frequency
  :: forall nt r ap
   . Newtype nt (Variant (frequency :: AudioParameter | r))
  => ToAudioParameter ap
  => ap
  -> nt
frequency = wrap <<< inj (Proxy :: Proxy "frequency") <<< toAudioParameter

gain
  :: forall nt r ap
   . Newtype nt (Variant (gain :: AudioParameter | r))
  => ToAudioParameter ap
  => ap
  -> nt
gain = wrap <<< inj (Proxy :: Proxy "gain") <<< toAudioParameter

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

offset
  :: forall nt r ap
   . Newtype nt (Variant (offset :: AudioParameter | r))
  => ToAudioParameter ap
  => ap
  -> nt
offset = wrap <<< inj (Proxy :: Proxy "offset") <<< toAudioParameter

onOff
  :: forall nt r ap
   . Newtype nt (Variant (onOff :: AudioOnOff | r))
  => ToAudioOnOff ap
  => ap
  -> nt
onOff = wrap <<< inj (Proxy :: Proxy "onOff") <<< toAudioOnOff


playbackRate
  :: forall nt r ap
   . Newtype nt (Variant (playbackRate :: AudioParameter | r))
  => ToAudioParameter ap
  => ap
  -> nt
playbackRate = wrap <<< inj (Proxy :: Proxy "playbackRate") <<< toAudioParameter