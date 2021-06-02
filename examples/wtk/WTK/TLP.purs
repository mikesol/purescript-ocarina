module WAGS.Example.WTK.TLP where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Functor.Indexed (ivoid)
import Data.List (List(..), (:))
import Data.Set as S
import Effect (Effect)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated ((@!>), iloop)
import WAGS.Control.Indexed (IxWAG, IxFrame)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Example.WTK.Types (Key(..), KeyInfo, MakeRenderingEnv, Trigger, KlavierType, fullKeyboard)
import WAGS.Graph.Optionals (gain_, sinOsc_)
import WAGS.Interpret (class AudioInterpret, FFIAudio)
import WAGS.Run (SceneI)

playKeys ::
  forall audio engine proof res.
  Monoid res =>
  AudioInterpret audio engine =>
  { currentTime :: Number
  , notesOff :: S.Set Int
  } ->
  List KeyInfo ->
  List KeyInfo ->
  IxWAG audio engine proof res KlavierType KlavierType Unit
playKeys rec Nil Nil = ipure unit

playKeys rec@{ currentTime, notesOff } Nil (a : b) = case a.k of
  K0 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k0: gain_ gain, osc0: sinOsc_ onOff freq }
    playKeys rec Nil b
  K1 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k1: gain_ gain, osc1: sinOsc_ onOff freq }
    playKeys rec Nil b
  K2 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k2: gain_ gain, osc2: sinOsc_ onOff freq }
    playKeys rec Nil b
  K3 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k3: gain_ gain, osc3: sinOsc_ onOff freq }
    playKeys rec Nil b
  K4 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k4: gain_ gain, osc4: sinOsc_ onOff freq }
    playKeys rec Nil b
  K5 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k5: gain_ gain, osc5: sinOsc_ onOff freq }
    playKeys rec Nil b
  K6 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k6: gain_ gain, osc6: sinOsc_ onOff freq }
    playKeys rec Nil b
  K7 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k7: gain_ gain, osc7: sinOsc_ onOff freq }
    playKeys rec Nil b
  K8 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k8: gain_ gain, osc8: sinOsc_ onOff freq }
    playKeys rec Nil b
  K9 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ ichange { k9: gain_ gain, osc9: sinOsc_ onOff freq }
    playKeys rec Nil b

playKeys rec (a : b) currentPlaying = case a.k of
  K0 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k0: gain_ gain, osc0: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K1 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k1: gain_ gain, osc1: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K2 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k2: gain_ gain, osc2: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K3 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k3: gain_ gain, osc3: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K4 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k4: gain_ gain, osc4: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K5 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k5: gain_ gain, osc5: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K6 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k6: gain_ gain, osc6: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K7 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k7: gain_ gain, osc7: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K8 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k8: gain_ gain, osc8: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K9 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ ichange { k9: gain_ gain, osc9: sinOsc_ onOff freq }
    playKeys rec b currentPlaying

type Accumulator
  = { currentKeys :: List KeyInfo
    , availableKeys :: List Key
    }

createFrame :: IxFrame (SceneI Trigger Unit) FFIAudio (Effect Unit) Frame0 Unit {} KlavierType Accumulator
createFrame _ =
  icreate fullKeyboard
    $> { currentKeys: Nil
      , availableKeys: K0 : K1 : K2 : K3 : K4 : K5 : K6 : K7 : K8 : K9 : Nil
      }

piece :: { makeRenderingEnv :: MakeRenderingEnv } -> Scene (SceneI Trigger Unit) FFIAudio (Effect Unit) Frame0 Unit
piece { makeRenderingEnv } =
  createFrame
    @!> iloop \{ time, trigger, active } { currentKeys, availableKeys } -> Ix.do
        let
          { notesOff
          , onsets
          , newCurrentKeys
          , newAvailableKeys
          , futureCurrentKeys
          , futureAvailableKeys
          } = makeRenderingEnv active trigger time availableKeys currentKeys
        ( playKeys
            { currentTime: time
            , notesOff
            }
            onsets
            newCurrentKeys
        )
          $> { currentKeys: futureCurrentKeys
            , availableKeys: futureAvailableKeys
            }
