module WAGS.Clock where

import Prelude

import FRP.Behavior (Behavior, behavior)
import FRP.Event (makeEvent, subscribe)
import WAGS.Interpret (getAudioClockTime)
import WAGS.Parameter (AudioNumeric(..), AudioOnOff(..), AudioParameter, AudioSudden(..), _linear, _numeric, _sudden)
import WAGS.WebAPI as WebAPI

type ACTime =
  { concreteTime :: Number, abstractTime :: Number, lookAhead :: Number }

type WriteHead (f :: Type -> Type) = f ACTime

writeHead :: Number -> WebAPI.AudioContext -> WriteHead Behavior
writeHead lookAhead ctx = behavior \eab -> makeEvent \k ->
  subscribe eab \ab -> do
    t <- getAudioClockTime ctx
    k (ab { concreteTime: t, abstractTime: t - lookAhead, lookAhead })

oo
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> AudioOnOff)
  -> f AudioOnOff
oo wh f = wh # map
  \{ concreteTime, abstractTime } ->
    let
      AudioOnOff { o, x } = f abstractTime
    in
      AudioOnOff { x, o: o + concreteTime }

fot
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> Number)
  -> f AudioNumeric
fot wh f = fot' wh (map (\n -> { o: 0.0, n: n }) f)

fot'
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> { n :: Number, o :: Number })
  -> f AudioNumeric
fot' wh f = wh # map
  \{ concreteTime, abstractTime } ->
    let
      { n, o } = f abstractTime
    in
      AudioNumeric { t: _linear, o: o + concreteTime, n }
