module WAGS.Run where

import Prelude

import Data.Array (uncons)
import Data.Maybe as DM
import Data.Typelevel.Num (D2)
import Effect (Effect)
import FRP.Event (Event, subscribe)
import WAGS.Control (constant_, speaker2, (:*), (:::*))
import WAGS.Core as C
import WAGS.Interpret (FFIAudioSnapshot, close, context, contextState, effectfulAudioInterpret, makeFFIAudioSnapshot)

run2_
  :: Array (C.Node D2 "" () Event (FFIAudioSnapshot -> Effect Unit))
  -> Effect (Effect Unit)
run2_ s = do
  ctx <- context
  ffi <- makeFFIAudioSnapshot ctx
  -- todo: make slightly more efficient - we don't need this extra gain, we can just pattern match
  -- like in Control.purs
  u <- subscribe
    ( speaker2
        ( case uncons s of
            DM.Nothing -> (constant_ 0.0 :* ([] :: Array (C.Node D2 "" () Event (FFIAudioSnapshot -> Effect Unit))))
            DM.Just { head, tail } -> (head :::* tail)
        )
        effectfulAudioInterpret
    )
    \f -> f ffi
  pure (u *> contextState ctx >>= \st -> when (st /= "closed") (close ctx))