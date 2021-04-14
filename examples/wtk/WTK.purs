module WAGS.Example.WTK where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Control.Alt ((<|>))
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List (List(..), (:), filter, length, drop, zipWith)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (subscribe)
import FRP.Event.MIDI (MIDIEvent(..), MIDIEventInTime, midi, midiAccess)
import Math (pow)
import Record (set)
import Type.Data.Peano (Succ)
import Type.Proxy (Proxy(..))
import WAGS (class Change, class Changes, AudioUnitRef, FFIAudio(..), FFIAudio', Focus(..), Frame0, FrameT, Gain(..), GetSetAP, Decorating', OnOff(..), Scene, SceneI, SinOsc(..), SingleEdge, Speaker(..), UniverseC, bufferToList, create, cursor, defaultGetSetAP, env, graph, loop, proof, run, start, withProof, (@>))
import WAGS.Change (ChangeInstruction(..), changes)
import WAGS.Control.Qualified as Ix
import WAGS.Interpret (class AudioInterpret)

data Key
  = K0
  | K1
  | K2
  | K3
  | K4
  | K5
  | K6
  | K7
  | K8
  | K9

playKeys ::
  forall k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 incoming env audio engine proof m currentIdx graph j skolems.
  Monad m =>
  AudioInterpret audio engine =>
  Change (SingleEdge k0) KeyUnit graph =>
  Change (SingleEdge k1) KeyUnit graph =>
  Change (SingleEdge k2) KeyUnit graph =>
  Change (SingleEdge k3) KeyUnit graph =>
  Change (SingleEdge k4) KeyUnit graph =>
  Change (SingleEdge k5) KeyUnit graph =>
  Change (SingleEdge k6) KeyUnit graph =>
  Change (SingleEdge k7) KeyUnit graph =>
  Change (SingleEdge k8) KeyUnit graph =>
  Change (SingleEdge k9) KeyUnit graph =>
  Changes incoming graph =>
  { graphProxy :: Proxy graph
  , audioRefs :: AudioUnitRef k0 /\ AudioUnitRef k1 /\ AudioUnitRef k2 /\ AudioUnitRef k3 /\ AudioUnitRef k4 /\ AudioUnitRef k5 /\ AudioUnitRef k6 /\ AudioUnitRef k7 /\ AudioUnitRef k8 /\ AudioUnitRef k9
  , currentTime :: Number
  , notesOff :: S.Set Int
  } ->
  incoming ->
  List KeyInfo ->
  List KeyInfo ->
  FrameT env audio engine proof m (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph (Succ j) skolems) Unit
-- finally add start/stop to change
playKeys rec incoming Nil Nil = changes incoming

playKeys rec@{ currentTime, notesOff } incoming Nil (a : b) = case a.k of
  K0 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k0)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K1 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k1)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K2 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k2)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K3 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k3)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K4 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k4)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K5 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k5)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K6 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k6)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K7 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k7)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K8 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k8)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b
  K9 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k9)) (if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime)) incoming) Nil b

playKeys rec incoming (a : b) currentPlaying = case a.k of
  K0 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k0)) a.startU) incoming) b currentPlaying
  K1 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k1)) a.startU) incoming) b currentPlaying
  K2 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k2)) a.startU) incoming) b currentPlaying
  K3 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k3)) a.startU) incoming) b currentPlaying
  K4 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k4)) a.startU) incoming) b currentPlaying
  K5 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k5)) a.startU) incoming) b currentPlaying
  K6 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k6)) a.startU) incoming) b currentPlaying
  K7 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k7)) a.startU) incoming) b currentPlaying
  K8 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k8)) a.startU) incoming) b currentPlaying
  K9 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k9)) a.startU) incoming) b currentPlaying


type KlavierType k0 k1 k2 k3 k4 k5 k6 k7 k8 k9
  = { k0 :: Decorating' k0
    , k1 :: Decorating' k1
    , k2 :: Decorating' k2
    , k3 :: Decorating' k3
    , k4 :: Decorating' k4
    , k5 :: Decorating' k5
    , k6 :: Decorating' k6
    , k7 :: Decorating' k7
    , k8 :: Decorating' k8
    , k9 :: Decorating' k9
    } ->
    Speaker
      ( Gain GetSetAP
          ( k0 KeyUnit
              /\ k1 KeyUnit
              /\ k2 KeyUnit
              /\ k3 KeyUnit
              /\ k4 KeyUnit
              /\ k5 KeyUnit
              /\ k6 KeyUnit
              /\ k7 KeyUnit
              /\ k8 KeyUnit
              /\ k9 KeyUnit
              /\ Unit
          )
      )

klavierIdentity =
  { k0: Identity
  , k1: Identity
  , k2: Identity
  , k3: Identity
  , k4: Identity
  , k5: Identity
  , k6: Identity
  , k7: Identity
  , k8: Identity
  , k9: Identity
  }

cursors =
  { k0: fullKeyboard (set (Proxy :: _ "k0") Focus klavierIdentity)
  , k1: fullKeyboard (set (Proxy :: _ "k1") Focus klavierIdentity)
  , k2: fullKeyboard (set (Proxy :: _ "k2") Focus klavierIdentity)
  , k3: fullKeyboard (set (Proxy :: _ "k3") Focus klavierIdentity)
  , k4: fullKeyboard (set (Proxy :: _ "k4") Focus klavierIdentity)
  , k5: fullKeyboard (set (Proxy :: _ "k5") Focus klavierIdentity)
  , k6: fullKeyboard (set (Proxy :: _ "k6") Focus klavierIdentity)
  , k7: fullKeyboard (set (Proxy :: _ "k7") Focus klavierIdentity)
  , k8: fullKeyboard (set (Proxy :: _ "k8") Focus klavierIdentity)
  , k9: fullKeyboard (set (Proxy :: _ "k9") Focus klavierIdentity)
  }

type KeyUnit
  = Gain GetSetAP (SinOsc GetSetAP)

fullKeyboard :: forall k0 k1 k2 k3 k4 k5 k6 k7 k8 k9. KlavierType k0 k1 k2 k3 k4 k5 k6 k7 k8 k9
fullKeyboard { k0, k1, k2, k3, k4, k5, k6, k7, k8, k9 } =
  Speaker
    ( Gain (defaultGetSetAP 1.0)
        ( k0 initialKey
            /\ k1 initialKey
            /\ k2 initialKey
            /\ k3 initialKey
            /\ k4 initialKey
            /\ k5 initialKey
            /\ k6 initialKey
            /\ k7 initialKey
            /\ k8 initialKey
            /\ k9 initialKey
            /\ unit
        )
    )

keyDur :: Number
keyDur = 1.6

initialKey :: KeyUnit
initialKey = Gain (defaultGetSetAP 0.0) (SinOsc Off (defaultGetSetAP 440.0))

keyToCps :: Int -> Number
keyToCps i = 440.0 * (2.0 `pow` ((toNumber i - 69.0) / 12.0))

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

dampen :: Number -> Number -> Number
dampen currentTime offTime
  | currentTime < offTime = 1.0
  | currentTime >= offTime + 0.2 = 0.0
  | otherwise = calcSlope offTime 1.0 (offTime + 0.2) 0.0 currentTime

asdr :: Number -> Number
asdr n
  | n <= 0.0 = 0.0
  | n < 0.25 = calcSlope 0.0 0.0 0.25 0.01 n
  | n < 0.5 = calcSlope 0.25 0.01 0.5 0.005 n
  | n < keyDur = calcSlope 0.5 0.005 keyDur 0.0 n
  | otherwise = 0.0

keyStart :: Number -> KeyUnit
keyStart cps = Gain (defaultGetSetAP 0.0) (SinOsc On (defaultGetSetAP cps))

keySustain :: Number -> Number -> Number -> KeyUnit
keySustain initialTime cps currentTime =
  Gain
    (defaultGetSetAP (asdr (currentTime - initialTime)))
    (SinOsc On (defaultGetSetAP cps))

keySustainOff :: Number -> Number -> Number -> Number -> KeyUnit
keySustainOff initialTime cps offTime currentTime =
  Gain
    (defaultGetSetAP (asdr (currentTime - initialTime) * dampen currentTime offTime))
    (SinOsc On (defaultGetSetAP cps))

keyEnd :: Number -> KeyUnit
keyEnd cps = Gain (defaultGetSetAP 0.0) (SinOsc Off (defaultGetSetAP cps))

midiEventsToOnsets :: List MIDIEvent -> (List Int /\ List Int)
midiEventsToOnsets = go Nil Nil
  where
  go accOn accOff Nil = accOn /\ accOff

  go accOn accOff (NoteOn _ note _ : b) = go (Cons note accOn) accOff b

  go accOn accOff (NoteOff _ note _ : b) = go accOn (Cons note accOff) b

  go accOn accOff (_ : b) = go accOn accOff b

type KeyInfo
  = { startU :: KeyUnit
    , endU :: KeyUnit
    , sustainU :: Number -> KeyUnit
    , startT :: Number
    , cps :: Number
    , keyDuration :: Number
    , i :: Int
    , k :: Key
    }

piece :: Scene (SceneI (List { time :: Instant, value :: MIDIEventInTime }) Unit) FFIAudio (Effect Unit) Frame0
piece =
  ( Ix.do
      start
      ivoid $ create $ fullKeyboard klavierIdentity
      k0 <- cursor $ cursors.k0
      k1 <- cursor $ cursors.k1
      k2 <- cursor $ cursors.k2
      k3 <- cursor $ cursors.k3
      k4 <- cursor $ cursors.k4
      k5 <- cursor $ cursors.k5
      k6 <- cursor $ cursors.k6
      k7 <- cursor $ cursors.k7
      k8 <- cursor $ cursors.k8
      k9 <- cursor $ cursors.k9
      myProof <- proof
      withProof myProof
        $ Right
            { audioRefs: k0 /\ k1 /\ k2 /\ k3 /\ k4 /\ k5 /\ k6 /\ k7 /\ k8 /\ k9
            , currentKeys: (Nil :: (List KeyInfo))
            , availableKeys: K0 : K1 : K2 : K3 : K4 : K5 : K6 : K7 : K8 : K9 : Nil
            }
  )
    @> loop
        ( \{ audioRefs, currentKeys, availableKeys } -> Ix.do
            { time, trigger, active } <- env
            graphProxy <- graph
            let
              notesOn /\ notesOffAsList =
                midiEventsToOnsets
                  (if active then (map _.value.event trigger) else Nil)

              notesOff = S.fromFoldable notesOffAsList

              onsets =
                zipWith
                  ( \i k ->
                      let
                        cps = keyToCps i
                      in
                        { sustainU: keySustain time cps
                        , startU: keyStart cps
                        , endU: keyEnd cps
                        , startT: time
                        , cps
                        , keyDuration: keyDur
                        , i
                        , k
                        }
                  )
                  notesOn
                  availableKeys

              newAvailableKeys = drop (length onsets) availableKeys

              newCurrentKeys =
                map
                  ( \rec ->
                      if S.member rec.i notesOff then
                        rec
                          { keyDuration = (time - rec.startT) + 0.2
                          , sustainU = keySustainOff rec.startT rec.cps time
                          }
                      else
                        rec
                  )
                  currentKeys
            ( playKeys
                { graphProxy
                , audioRefs
                , currentTime: time
                , notesOff
                }
                unit
                onsets
                newCurrentKeys
            )
              $> { audioRefs
                , currentKeys: (filter (\{ startT, keyDuration, i } -> time - startT <= keyDuration) newCurrentKeys) <> onsets
                , availableKeys: newAvailableKeys <> (map _.k (filter (\{ startT, keyDuration, i } -> time - startT > keyDuration) newCurrentKeys))
                }
        )

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm = let fOf initialTime = mkCofree initialTime \adj -> fOf $ max 10 (initialTime - adj) in fOf 20

myRun :: FFIAudio' -> Effect Unit
myRun ffiAudio =
  launchAff_ do
    midAcc <- toAffE midiAccess
    -- alt Nil for thunk
    let
      trigger = (bufferToList 5 (midi midAcc)) <|> pure Nil
    liftEffect
      $ do
          unsubscribe <-
            subscribe
              (run { easingAlgorithm } (FFIAudio ffiAudio) trigger (pure unit) piece)
              (const $ pure unit)
          -- swallow unsubscribe
          pure unit

main :: Effect Unit
main = pure unit
