module WAGS.Run where

import Prelude

import Data.Typelevel.Num (D2)
import Effect (Effect)
import FRP.Event (Event, subscribe)
import WAGS.Control (speaker2, singleton)
import WAGS.Core as C
import WAGS.Interpret (FFIAudioSnapshot, close, context, contextState, effectfulAudioInterpret, makeFFIAudioSnapshot)
import WAGS.WebAPI (AudioContext)

class Run2_ f where
  run2_
    :: f D2 "" () Event (FFIAudioSnapshot -> Effect Unit)
    -> Effect (Effect Unit)
  run2
    :: AudioContext
    -> f D2 "" () Event (FFIAudioSnapshot -> Effect Unit)
    -> Effect (Effect Unit)

instance Run2_ C.Node where
  run2_ n = run2_ (singleton n)
  run2 ctx n = run2 ctx (singleton n)

instance Run2_ C.AudioInput where
  run2_ s = do
    ctx <- context
    u <- run2 ctx s
    pure (u *> contextState ctx >>= \st -> when (st /= "closed") (close ctx))
  run2 ctx s = do
    ffi <- makeFFIAudioSnapshot ctx
    u <- subscribe (speaker2 s effectfulAudioInterpret)
      \f -> f ffi
    pure u