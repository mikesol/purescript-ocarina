module WAGS.Common.Parameters.PeriodicOsc where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos)
import Data.Variant (inj)
import Data.Vec (Vec, toArray)
import Type.Prelude (Proxy(..))
import WAGS.Core as Core
import WAGS.WebAPI (BrowserPeriodicWave)

data PeriodicOscOptions = PeriodicOscOptions

instance
  ConvertOption PeriodicOscOptions
    "frequency"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

class PeriodicOscSpecable i where
  toPeriodicOscSpec :: i -> Core.PeriodicOscSpec

instance PeriodicOscSpecable BrowserPeriodicWave where
  toPeriodicOscSpec = Core.PeriodicOscSpec <<< inj (Proxy :: _ "wave")

instance Pos n => PeriodicOscSpecable (Vec n Number /\ Vec n Number) where
  toPeriodicOscSpec (real /\ img) = Core.PeriodicOscSpec $ inj (Proxy :: _ "realImg") $ Core.RealImg { real: toArray real, img: toArray img }

instance
  PeriodicOscSpecable i =>
  ConvertOption PeriodicOscOptions
    "spec"
    i
    Core.PeriodicOscSpec where
  convertOption _ _ = toPeriodicOscSpec

type PeriodicOscAll =
  ( frequency :: Core.InitialAudioParameter
  , spec :: Core.PeriodicOscSpec
  )

defaultPeriodicOsc :: {}
defaultPeriodicOsc = {}

class InitialPeriodicOsc i where
  toInitializePeriodicOsc :: i -> Core.InitializePeriodicOsc

instance InitialPeriodicOsc Core.InitializePeriodicOsc where
  toInitializePeriodicOsc = identity

instance
  ConvertOptionsWithDefaults PeriodicOscOptions {} { | provided }
    { | PeriodicOscAll } =>
  InitialPeriodicOsc { | provided } where
  toInitializePeriodicOsc provided = Core.InitializePeriodicOsc
    (convertOptionsWithDefaults PeriodicOscOptions defaultPeriodicOsc provided)
