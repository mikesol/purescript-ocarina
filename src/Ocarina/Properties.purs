module Ocarina.Properties where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))
import Ocarina.Core (class ToAudioOnOff, class ToAudioParameter, AudioOnOff, AudioParameter, toAudioOnOff, toAudioParameter)
import Ocarina.WebAPI (BrowserAudioBuffer)

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

delayTime
  :: forall nt r ap l p
   . Newtype nt (Variant (delayTime :: AudioParameter l p | r))
  => ToAudioParameter ap l p
  => ap
  -> nt
delayTime = wrap <<< inj (Proxy :: Proxy "delayTime") <<< toAudioParameter

frequency
  :: forall nt r ap l p
   . Newtype nt (Variant (frequency :: AudioParameter l p | r))
  => ToAudioParameter ap l p
  => ap
  -> nt
frequency = wrap <<< inj (Proxy :: Proxy "frequency") <<< toAudioParameter

q
  :: forall nt r ap l p
   . Newtype nt (Variant (q :: AudioParameter l p | r))
  => ToAudioParameter ap l p
  => ap
  -> nt
q = wrap <<< inj (Proxy :: Proxy "q") <<< toAudioParameter

gain
  :: forall nt r ap l p
   . Newtype nt (Variant (gain :: AudioParameter l p | r))
  => ToAudioParameter ap l p
  => ap
  -> nt
gain = wrap <<< inj (Proxy :: Proxy "gain") <<< toAudioParameter

duration
  :: forall nt r
   . Newtype nt (Variant (duration :: Maybe Number | r))
  => Maybe Number
  -> nt
duration = wrap <<< inj (Proxy :: Proxy "duration")

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
  :: forall nt r ap l p
   . Newtype nt (Variant (offset :: AudioParameter l p | r))
  => ToAudioParameter ap l p
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
  :: forall nt r ap l p
   . Newtype nt (Variant (playbackRate :: AudioParameter l p | r))
  => ToAudioParameter ap l p
  => ap
  -> nt
playbackRate = wrap <<< inj (Proxy :: Proxy "playbackRate") <<< toAudioParameter