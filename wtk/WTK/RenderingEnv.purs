module WAGS.Example.WTK.RenderingEnv where

import Prelude

import Data.Int (toNumber)
import Data.List (List(..), (:), filter, length, drop, zipWith)
import Data.Set as S
import Data.Tuple.Nested ((/\), type (/\))
import FRP.Event.MIDI (MIDIEvent(..))
import Math (pow)
import WAGS.Example.WTK.Types (KeyUnit, MakeRenderingEnv)
import WAGS.Graph.Constructors (Gain(..), OnOff(..), SinOsc(..))
import WAGS.Graph.Optionals (defaultGetSetAP)

keyDur :: Number
keyDur = 1.6

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

makeRenderingEnv :: MakeRenderingEnv
makeRenderingEnv active trigger time availableKeys currentKeys =
  { notesOff
  , onsets
  , newCurrentKeys
  , newAvailableKeys
  , futureCurrentKeys: (filter (\{ startT, keyDuration, i } -> time - startT <= keyDuration) newCurrentKeys) <> onsets
  , futureAvailableKeys: newAvailableKeys <> (map _.k (filter (\{ startT, keyDuration, i } -> time - startT > keyDuration) newCurrentKeys))
  }
  where
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
