module WAGS.Run where

import Prelude

import Data.Typelevel.Num (D2)
import Effect (Effect)
import FRP.Event (Event, keepLatest, subscribe)
import WAGS.Control (speaker2)
import WAGS.Core (mix)
import WAGS.Core as C
import WAGS.Interpret (FFIAudioSnapshot, context, effectfulAudioInterpret, makeFFIAudioSnapshot)
import WAGS.WebAPI (AudioContext)

run2_
  :: (forall lock. Array (C.Node D2 lock (FFIAudioSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run2_ s = do
  ctx <- context
  run2 ctx s

run2
  :: AudioContext
  -> (forall lock. Array (C.Node D2 lock (FFIAudioSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run2 ctx s = do
    ffi <- makeFFIAudioSnapshot ctx
    u <- subscribe (speaker2 s effectfulAudioInterpret)
      \f -> f ffi
    pure u

run2e_
  :: (forall lock. Event (Array (C.Node D2 lock (FFIAudioSnapshot -> Effect Unit))))
  -> Effect (Effect Unit)
run2e_ s = do
  ctx <- context
  run2e ctx s

run2e
  :: AudioContext
  -> (forall lock. Event (Array (C.Node D2 lock (FFIAudioSnapshot -> Effect Unit))))
  -> Effect (Effect Unit)
run2e ctx s = do
    ffi <- makeFFIAudioSnapshot ctx
    u <- subscribe (speaker2 (keepLatest (map mix s)) effectfulAudioInterpret)
      \f -> f ffi
    pure u