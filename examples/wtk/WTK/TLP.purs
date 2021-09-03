module WAGS.Example.WTK.TLP where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.List (List(..), (:))
import Data.Set as S
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated ((@!>), iloop)
import WAGS.Control.Indexed (IxWAG, IxFrame)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Example.WTK.Types (Key(..), KeyInfo, MakeRenderingEnv, Trigger, KlavierType, fullKeyboard)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Run (RunAudio, RunEngine, SceneI(..))

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
playKeys _ Nil Nil = ipure unit

playKeys rec@{ currentTime } Nil (a : b) = case a.k of
  K0 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k0: gain, osc0: { onOff, freq } }
    playKeys rec Nil b
  K1 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k1: gain, osc1: { onOff, freq } }
    playKeys rec Nil b
  K2 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k2: gain, osc2: { onOff, freq } }
    playKeys rec Nil b
  K3 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k3: gain, osc3: { onOff, freq } }
    playKeys rec Nil b
  K4 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k4: gain, osc4: { onOff, freq } }
    playKeys rec Nil b
  K5 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k5: gain, osc5: { onOff, freq } }
    playKeys rec Nil b
  K6 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k6: gain, osc6: { onOff, freq } }
    playKeys rec Nil b
  K7 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k7: gain, osc7: { onOff, freq } }
    playKeys rec Nil b
  K8 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k8: gain, osc8: { onOff, freq } }
    playKeys rec Nil b
  K9 -> Ix.do
    let
      { gain, freq, onOff } = if currentTime - a.startT > a.keyDuration then a.endU else a.sustainU currentTime
    ichange { k9: gain, osc9: { onOff, freq } }
    playKeys rec Nil b

playKeys rec (a : b) currentPlaying = case a.k of
  K0 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k0: gain, osc0: { onOff, freq } }
    playKeys rec b currentPlaying
  K1 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k1: gain, osc1: { onOff, freq } }
    playKeys rec b currentPlaying
  K2 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k2: gain, osc2: { onOff, freq } }
    playKeys rec b currentPlaying
  K3 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k3: gain, osc3: { onOff, freq } }
    playKeys rec b currentPlaying
  K4 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k4: gain, osc4: { onOff, freq } }
    playKeys rec b currentPlaying
  K5 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k5: gain, osc5: { onOff, freq } }
    playKeys rec b currentPlaying
  K6 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k6: gain, osc6: { onOff, freq } }
    playKeys rec b currentPlaying
  K7 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k7: gain, osc7: { onOff, freq } }
    playKeys rec b currentPlaying
  K8 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k8: gain, osc8: { onOff, freq } }
    playKeys rec b currentPlaying
  K9 -> Ix.do
    let
      { gain, freq, onOff } = a.startU
    ichange { k9: gain, osc9: { onOff, freq } }
    playKeys rec b currentPlaying

type Accumulator
  = { currentKeys :: List KeyInfo
    , availableKeys :: List Key
    }

createFrame :: IxFrame (SceneI Trigger Unit ()) RunAudio RunEngine Frame0 Unit {} KlavierType Accumulator
createFrame _ =
  icreate fullKeyboard
    $> { currentKeys: Nil
      , availableKeys: K0 : K1 : K2 : K3 : K4 : K5 : K6 : K7 : K8 : K9 : Nil
      }

piece :: { makeRenderingEnv :: MakeRenderingEnv } -> Scene (SceneI Trigger Unit ()) RunAudio RunEngine Frame0 Unit
piece { makeRenderingEnv } =
  createFrame
    @!> iloop \(SceneI { time, trigger }) { currentKeys, availableKeys } -> Ix.do
        let
          { notesOff
          , onsets
          , newCurrentKeys
          , futureCurrentKeys
          , futureAvailableKeys
          } = makeRenderingEnv trigger time availableKeys currentKeys
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
