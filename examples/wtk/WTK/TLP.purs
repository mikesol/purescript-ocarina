module WAGS.Example.WTK.TLP where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.List (List(..), (:))
import Data.Set as S
import Effect (Effect)
import Type.Proxy (Proxy)
import WAGS.Change (change)
import WAGS.Control.Functions (env, graph, loop, proof, start, withProof, (@|>))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame0, FrameT, Scene, Frame)
import WAGS.Create (create)
import WAGS.Example.WTK.Types (Key(..), KeyInfo, MakeRenderingEnv, Trigger, KlavierType, fullKeyboard)
import WAGS.Graph.Optionals (gain_, sinOsc_)
import WAGS.Interpret (class AudioInterpret, FFIAudio)
import WAGS.Run (SceneI)

playKeys ::
  forall env audio engine proof m res graph.
  Monad m =>
  Monoid res =>
  AudioInterpret audio engine =>
  { graphProxy :: Proxy graph
  , currentTime :: Number
  , notesOff :: S.Set Int
  } ->
  List KeyInfo ->
  List KeyInfo ->
  FrameT env audio engine proof m res KlavierType KlavierType Unit
playKeys rec Nil Nil = WAGS.do
  pr <- proof
  withProof pr unit

playKeys rec@{ currentTime, notesOff } Nil (a : b) = case a.k of
  K0 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k0: gain_ gain, osc0: sinOsc_ onOff freq }
    playKeys rec Nil b
  K1 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k1: gain_ gain, osc1: sinOsc_ onOff freq }
    playKeys rec Nil b
  K2 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k2: gain_ gain, osc2: sinOsc_ onOff freq }
    playKeys rec Nil b
  K3 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k3: gain_ gain, osc3: sinOsc_ onOff freq }
    playKeys rec Nil b
  K4 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k4: gain_ gain, osc4: sinOsc_ onOff freq }
    playKeys rec Nil b
  K5 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k5: gain_ gain, osc5: sinOsc_ onOff freq }
    playKeys rec Nil b
  K6 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k6: gain_ gain, osc6: sinOsc_ onOff freq }
    playKeys rec Nil b
  K7 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k7: gain_ gain, osc7: sinOsc_ onOff freq }
    playKeys rec Nil b
  K8 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k8: gain_ gain, osc8: sinOsc_ onOff freq }
    playKeys rec Nil b
  K9 -> WAGS.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ivoid $ change { k9: gain_ gain, osc9: sinOsc_ onOff freq }
    playKeys rec Nil b

playKeys rec (a : b) currentPlaying = case a.k of
  K0 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k0: gain_ gain, osc0: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K1 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k1: gain_ gain, osc1: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K2 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k2: gain_ gain, osc2: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K3 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k3: gain_ gain, osc3: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K4 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k4: gain_ gain, osc4: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K5 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k5: gain_ gain, osc5: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K6 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k6: gain_ gain, osc6: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K7 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k7: gain_ gain, osc7: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K8 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k8: gain_ gain, osc8: sinOsc_ onOff freq }
    playKeys rec b currentPlaying
  K9 -> WAGS.do
    let
      { gain, freq, onOff } = a.startU
    ivoid $ change { k9: gain_ gain, osc9: sinOsc_ onOff freq }
    playKeys rec b currentPlaying

type Accumulator
  = { currentKeys :: List KeyInfo
    , availableKeys :: List Key
    }

createFrame :: Frame (SceneI Trigger Unit) FFIAudio (Effect Unit) Frame0 {} KlavierType Accumulator
createFrame = WAGS.do
  start
  create fullKeyboard
    $> { currentKeys: Nil
      , availableKeys: K0 : K1 : K2 : K3 : K4 : K5 : K6 : K7 : K8 : K9 : Nil
      }

piece :: { makeRenderingEnv :: MakeRenderingEnv } -> Scene (SceneI Trigger Unit) FFIAudio (Effect Unit) Frame0
piece { makeRenderingEnv } =
  createFrame
    @|> loop
        ( \{ currentKeys, availableKeys } -> WAGS.do
            { time, trigger, active } <- env
            graphProxy <- graph
            let
              { notesOff
              , onsets
              , newCurrentKeys
              , newAvailableKeys
              , futureCurrentKeys
              , futureAvailableKeys
              } = makeRenderingEnv active trigger time availableKeys currentKeys
            ( playKeys
                { graphProxy
                , currentTime: time
                , notesOff
                }
                onsets
                newCurrentKeys
            )
              $> { currentKeys: futureCurrentKeys
                , availableKeys: futureAvailableKeys
                }
        )
