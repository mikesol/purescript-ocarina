module WAGS.Example.HelloWorld where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Functor.Indexed (ivoid)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import FRP.Event (subscribe)
import Math (pi, sin)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Graph.AudioUnit (TGain, TSinOsc, TSpeaker)
import WAGS.Graph.Optionals (CGain, CSpeaker, CSinOsc, gain, sinOsc, speaker)
import WAGS.Interpret (FFIAudio(..), FFIAudio')
import WAGS.Run (SceneI, run)

type SceneTemplate
  = CSpeaker
      { gain0 :: CGain { sin0 :: CSinOsc }
      , gain1 :: CGain { sin1 :: CSinOsc }
      , gain2 :: CGain { sin2 :: CSinOsc }
      , gain3 :: CGain { sin3 :: CSinOsc }
      }

type SceneType
  = { speaker :: TSpeaker /\ { gain0 :: Unit, gain1 :: Unit, gain2 :: Unit, gain3 :: Unit }
    , gain0 :: TGain /\ { sin0 :: Unit }
    , sin0 :: TSinOsc /\ {}
    , gain1 :: TGain /\ { sin1 :: Unit }
    , sin1 :: TSinOsc /\ {}
    , gain2 :: TGain /\ { sin2 :: Unit }
    , sin2 :: TSinOsc /\ {}
    , gain3 :: TGain /\ { sin3 :: Unit }
    , sin3 :: TSinOsc /\ {}
    }

scene :: Number -> SceneTemplate
scene time =
  let
    rad = pi * time
  in
    speaker
      { gain0: gain 0.1 { sin0: sinOsc (440.0 + (10.0 * sin (2.3 * rad))) }
      , gain1: gain 0.25 { sin1: sinOsc (235.0 + (10.0 * sin (1.7 * rad))) }
      , gain2: gain 0.2 { sin2: sinOsc (337.0 + (10.0 * sin rad)) }
      , gain3: gain 0.1 { sin3: sinOsc (530.0 + (19.0 * (5.0 * sin rad))) }
      }

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0 Unit
piece = (_.time >>> scene >>> icreate) @!> iloop \{ time } _ -> ivoid $ ichange (scene time)

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

myRun :: FFIAudio' -> Effect (Effect Unit)
myRun ffiAudio =
  subscribe
    (run (pure unit) (pure unit) { easingAlgorithm } (FFIAudio ffiAudio) piece)
    (const $ pure unit)

main :: Effect Unit
main = pure unit
