module WAGS.Run where

import Prelude

import Data.Typelevel.Num (D2)
import Effect (Effect)
import FRP.Event (Event, subscribe)
import WAGS.Control (speaker2)
import WAGS.Core as C
import WAGS.Interpret (FFIAudioSnapshot, close, context, contextState, effectfulAudioInterpret, makeFFIAudioSnapshot)
import WAGS.WebAPI (AudioContext)

run2_
  :: (forall lock. Array (C.Node D2 lock Event (FFIAudioSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run2_ s = do
  ctx <- context
  u <- run2 ctx s
  pure (u *> contextState ctx >>= \st -> when (st /= "closed") (close ctx))

run2
  :: AudioContext
  -> (forall lock. Array (C.Node D2 lock Event (FFIAudioSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run2 ctx s = do
    ffi <- makeFFIAudioSnapshot ctx
    u <- subscribe (speaker2 s effectfulAudioInterpret)
      \f -> f ffi
    pure u