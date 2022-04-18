module WAGS.Clock where

import Prelude

import Data.Lens (over, view)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (makeEvent, subscribe)
import Type.Proxy (Proxy(..))
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

cp :: forall f. Applicative f => Number -> f AudioParameter
cp n = pure (_sudden (AudioSudden { n }))

oo
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> AudioOnOff)
  -> f AudioOnOff
oo wh f = wh # map
  \{ concreteTime, abstractTime } ->
    let
      AudioOnOff { o, n } = f abstractTime
    in
      AudioOnOff { n, o: o + concreteTime }

at
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> Number)
  -> f AudioParameter
at wh f = at' wh (map (\n -> { o: 0.0, n: n }) f)

at'
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> { n :: Number, o :: Number })
  -> f AudioParameter

at' wh f = map _numeric (at_' wh f)

at_
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> Number)
  -> f AudioNumeric
at_ wh f = at_' wh (map (\n -> { o: 0.0, n: n }) f)

at_'
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> { n :: Number, o :: Number })
  -> f AudioNumeric
at_' wh f = wh # map
  \{ concreteTime, abstractTime } ->
    let
      { n, o } = f abstractTime
    in
      AudioNumeric { t: _linear, o: o + concreteTime, n }

uat_
  :: ACTime
  -> (Number -> Number)
  -> AudioNumeric
uat_ wh f = uat_' wh (map (\n -> { o: 0.0, n: n }) f)

uat_'
  :: ACTime
  -> (Number -> { n :: Number, o :: Number })
  -> AudioNumeric
uat_' { concreteTime, abstractTime } f =
  let
    { n, o } = f abstractTime
  in
    AudioNumeric { t: _linear, o: o + concreteTime, n }

ovnn :: (Number -> Number) -> AudioNumeric -> AudioNumeric
ovnn = over (unto AudioNumeric <<< prop (Proxy :: Proxy "n"))

vwnn :: AudioNumeric -> Number
vwnn = view (unto AudioNumeric <<< prop (Proxy :: Proxy "n"))
