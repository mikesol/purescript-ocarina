module WAGS.Run where

import Prelude

import Data.Typelevel.Num (D2)
import Effect (Effect)
import FRP.Event (Event, subscribe)
import WAGS.Control (speaker2, singleton)
import WAGS.Core as C
import WAGS.Interpret (FFIAudioSnapshot, close, context, contextState, effectfulAudioInterpret, makeFFIAudioSnapshot)

class Run2_ f where
  run2_
    :: f D2 "" () Event (FFIAudioSnapshot -> Effect Unit)
    -> Effect (Effect Unit)

instance Run2_ C.Node where
  run2_ n = run2_ (singleton n)
instance Run2_ C.AudioInput where
  run2_ s = do
    ctx <- context
    ffi <- makeFFIAudioSnapshot ctx
    u <- subscribe (speaker2 s effectfulAudioInterpret)
      \f -> f ffi
    pure (u *> contextState ctx >>= \st -> when (st /= "closed") (close ctx))