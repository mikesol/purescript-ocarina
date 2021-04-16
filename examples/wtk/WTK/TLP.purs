module WAGS.Example.WTK.TLP where

import Prelude
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.List (List(..), (:))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Type.Data.Peano (Succ)
import Type.Proxy (Proxy(..))
import WAGS (class Change, class Changes, AudioUnitRef, FFIAudio, Frame0, FrameT, Scene, SceneI, SingleEdge, UniverseC, create, cursor, env, graph, loop, proof, start, withProof, (@>))
import WAGS.Change (ChangeInstruction(..), changes)
import WAGS.Control.Qualified as Ix
import WAGS.Example.WTK.Types (Key(..), MakeRenderingEnv, KeyInfo, KeyUnit, Trigger, cursors, fullKeyboard, klavierIdentity)
import WAGS.Interpret (class AudioInterpret)

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

piece :: { makeRenderingEnv :: MakeRenderingEnv } -> Scene (SceneI Trigger Unit) FFIAudio (Effect Unit) Frame0
piece { makeRenderingEnv } =
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
              { notesOff
              , onsets
              , newCurrentKeys
              , newAvailableKeys
              , futureCurrentKeys
              , futureAvailableKeys
              } = makeRenderingEnv active trigger time availableKeys currentKeys
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
                , currentKeys: futureCurrentKeys
                , availableKeys: futureAvailableKeys
                }
        )
