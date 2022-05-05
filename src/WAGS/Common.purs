module WAGS.Common where

import Prelude

import Control.Alt ((<|>))
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos)
import Data.Variant (inj, match)
import Data.Variant.Maybe (Maybe, just, nothing)
import Data.Vec (Vec, toArray)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals, proof)
import Type.Proxy (Proxy(..))
import WAGS.Core (ChannelCountMode(..), ChannelInterpretation(..), Oversample, PeriodicOscSpec(..), Po2(..), RealImg(..), _twoX)
import WAGS.Core as Core
import WAGS.WebAPI (AnalyserNodeCb(..), BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

-- IIRFilter
class InitialIIRFilter i feedforward feedback where
  toInitializeIIRFilter :: forall proxy. i -> proxy feedforward -> proxy feedback -> (Core.InitializeIIRFilter feedforward feedback)

instance
  ( TypeEquals feedforwardI feedforwardO
  , TypeEquals feedbackI feedbackO
  ) =>
  InitialIIRFilter (Core.InitializeIIRFilter feedforwardI feedbackI) feedforwardO feedbackO where
  toInitializeIIRFilter (Core.InitializeIIRFilter { feedforward, feedback }) _ _ = Core.InitializeIIRFilter
    { feedforward: proof (coerce feedforward), feedback: proof (coerce feedback) }

instance
  ( TypeEquals feedforwardI feedforwardO
  , TypeEquals feedbackI feedbackO
  ) =>
  InitialIIRFilter (Vec feedforwardI Number /\ Vec feedbackI Number) feedforwardO feedbackO where
  toInitializeIIRFilter (feedforward /\ feedback) _ _ = Core.InitializeIIRFilter { feedforward: proof (coerce feedforward), feedback: proof (coerce feedback) }

-- StereoPanner
class InitialStereoPanner i where
  toInitializeStereoPanner :: i -> Core.InitializeStereoPanner

instance InitialStereoPanner Core.InitializeStereoPanner where
  toInitializeStereoPanner = identity

instance InitialStereoPanner Number where
  toInitializeStereoPanner = Core.InitializeStereoPanner <<< { pan: _ }

-- Recorder
class InitialRecorder i where
  toInitializeRecorder :: i -> Core.InitializeRecorder

instance InitialRecorder Core.InitializeRecorder where
  toInitializeRecorder = identity

instance InitialRecorder MediaRecorderCb where
  toInitializeRecorder = Core.InitializeRecorder <<< { cb: _ }

-- WaveShaper

data WaveShaperOptions = WaveShaperOptions

instance
  ConvertOption WaveShaperOptions
    "curve"
    BrowserFloatArray
    BrowserFloatArray where
  convertOption _ _ = identity

instance
  ConvertOption WaveShaperOptions
    "oversample"
    Oversample
    Oversample where
  convertOption _ _ = identity

type WaveShaperOptional =
  ( oversample :: Oversample
  )

type WaveShaperAll =
  ( curve :: BrowserFloatArray
  | WaveShaperOptional
  )

defaultWaveShaper :: { | WaveShaperOptional }
defaultWaveShaper =
  { oversample: _twoX }

class InitialWaveShaper i where
  toInitializeWaveShaper :: i -> Core.InitializeWaveShaper

instance InitialWaveShaper Core.InitializeWaveShaper where
  toInitializeWaveShaper = identity

instance InitialWaveShaper BrowserFloatArray where
  toInitializeWaveShaper = toInitializeWaveShaper <<< { curve: _ }

instance
  ConvertOptionsWithDefaults WaveShaperOptions { | WaveShaperOptional } { | provided }
    { | WaveShaperAll } =>
  InitialWaveShaper { | provided } where
  toInitializeWaveShaper provided = Core.InitializeWaveShaper
    (convertOptionsWithDefaults WaveShaperOptions defaultWaveShaper provided)

-- resolveAU

resolveAU :: forall lock payload. Core.AudioInterpret payload -> (Core.FFIAudioParameter -> payload) -> Core.AudioParameter lock payload -> Event payload
resolveAU = go
  where
  cncl = Core.FFIAudioParameter <<< inj (Proxy :: _ "cancel")
  ev = Core.FFIAudioParameter <<< inj (Proxy :: _ "envelope")
  nmc = Core.FFIAudioParameter <<< inj (Proxy :: _ "numeric")
  sdn = Core.FFIAudioParameter <<< inj (Proxy :: _ "sudden")
  ut = Core.FFIAudioParameter <<< inj (Proxy :: _ "unit")
  go di@(Core.AudioInterpret { ids }) f (Core.AudioParameter a) = match
    { numeric: bang <<< f <<< nmc
    , envelope: bang <<< f <<< ev
    , cancel: bang <<< f <<< cncl
    , sudden: bang <<< f <<< sdn
    , unit: \(Core.AudioUnit { u }) ->
        let
          Core.Node n = u
        in
          makeEvent \k -> do
            newScope <- ids
            av <- AVar.empty
            subscribe
              ( n { parent: nothing, scope: newScope, raiseId: \x -> void $ AVar.tryPut x av } di <|> makeEvent \k2 -> do
                  void $ AVar.take av case _ of
                    Left e -> throwException e
                    -- only do the connection if not silence
                    Right i -> k2 (f (ut (Core.FFIAudioUnit { i })))
                  pure (pure unit)
              )
              k
    }
    a
