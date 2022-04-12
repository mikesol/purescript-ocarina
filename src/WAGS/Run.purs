module WAGS.Run where

import Prelude

import Data.Typelevel.Num (D2)
import Effect (Effect)
import FRP.Event (Event, subscribe)
import WAGS.Control (gain_, singleton, speaker2)
import WAGS.Core as C
import WAGS.Interpret (FFIAudioSnapshot, close, context, effectfulAudioInterpret, makeFFIAudioSnapshot)

run2_
  :: Array (C.Node D2 "" () Event (FFIAudioSnapshot -> Effect Unit))
  -> Effect (Effect Unit)
run2_ s = do
  ctx <- context
  ffi <- makeFFIAudioSnapshot ctx
  -- todo: make slightly more efficient - we don't need this extra gain, we can just pattern match
  -- like in Control.purs
  u <- subscribe (speaker2 (singleton (gain_ 1.0 s)) effectfulAudioInterpret)
    \f -> f ffi
  pure (u *> close ctx)