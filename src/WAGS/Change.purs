module WAGS.Change
  ( class Change
  , class ChangeP
  , changeP
  , changes
  , class Changes
  , class ChangeInstructions
  , ChangeInstruction(..)
  , class Modify
  , class Modify'
  , class ModifyRes
  , class SetterVal
  , change
  , change'
  , changeAt
  , changeInstructions
  , setterVal
  ) where

import Prelude
import Control.Monad.State (gets, modify_)
import Control.Plus (empty)
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.Lens as Lens
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Peano (Succ)
import Type.Proxy (Proxy(..))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (FrameT, unsafeFrame, unsafeUnframe)
import WAGS.Create (class Create)
import WAGS.Graph.Constructors (OnOff(..))
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Decorators (Focus(..), IgnoreMe)
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam, param)
import WAGS.Interpret (class AudioInterpret, setAttack, setBuffer, setDelay, setFrequency, setGain, setKnee, setLoopEnd, setLoopStart, setOff, setOffset, setOn, setPan, setPeriodicOsc, setPlaybackRate, setQ, setRatio, setRelease, setThreshold)
import WAGS.Rendered (AnAudioUnit(..))
import WAGS.Universe.AudioUnit (AudioUnitRef)
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinSub, class BinToInt, Bits, Ptr, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.BinN (D0)
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, Graph, InitialGraph)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class ToSkolemizedFunction, SkolemListCons, SkolemPairC, toSkolemizedFunction)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Validation (class AltEdgeProfile, class NodeListAppend, class TerminalIdentityEdge)

-- | Change all of the audio nodes in edge profile `p` using the template laid out by type `a` for `graph`.
-- |
-- | ```purescript
-- | myCursor <- cursor (Speaker (Gain 1.0 (SinOsc 440.0 /\ Focus (SinOsc 330.0) /\ Unit)))
-- | change' (asEdgeProfile myCursor) (SinOsc 332.0)
-- | ```
-- |
-- | Note that `change'` increments the `changeBit` in the universe by 1, aka `Succ`.
-- | This use of inductive types in an indexed bind operation guarantees that we never go
-- | "back in time" after having changed an audio graph.
class Change (p :: EdgeProfile) (a :: Type) (graph :: Graph) (b :: Type) | p a graph -> b where
  change' :: forall env audio engine proof m res ptr changeBit skolems. Monad m => AudioInterpret audio engine => Proxy p -> a -> FrameT env audio engine proof m res (UniverseC ptr graph changeBit skolems) (UniverseC ptr graph (Succ changeBit) skolems) b

-- | Similar to `change'`, but starting from an `AudioReference ptr` (the result of `cursor`) instead of starting from an edge profile.
changeAt ::
  forall ptr a env audio engine proof m res currentIdx graph changeBit skolems.
  Monad m =>
  AudioInterpret audio engine =>
  Change (SingleEdge ptr) a graph =>
  AudioUnitRef ptr -> a -> FrameT env audio engine proof m res (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx graph (Succ changeBit) skolems) Unit
changeAt _ = change' (Proxy :: _ (SingleEdge ptr))

-- | Similar to `change'`, but starting from the top-level node, which is usually a `Speaker`.
change ::
  forall edge m res a currentIdx graph changeBit skolems env audio engine proof.
  Monad m =>
  AudioInterpret audio engine =>
  TerminalIdentityEdge graph edge =>
  Change edge a graph =>
  a -> FrameT env audio engine proof m res (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx graph (Succ changeBit) skolems) Unit
change = change' (Proxy :: _ edge)

-- | Rolls multiple changes into a single increment of the `changeBit`. This is useful when writing loops. A loop may have many or no changes, and this allows all of them to be executed in a single transaction. As an example, see `examples/wtk/WTK/TLP.purs`. In the `playKeys` function, each finger may potentially change, and each change is rolled into the `a` value that is ultimately passed to `changes`.
class Changes (a :: Type) (g :: Graph) (b :: Type) | a g -> b where
  changes :: forall env audio engine proof m res ptr changeBit skolems. Monad m => AudioInterpret audio engine => a -> FrameT env audio engine proof m res (UniverseC ptr g changeBit skolems) (UniverseC ptr g (Succ changeBit) skolems) b

-- | A term that can be coerced to an setter for a control-rate audio parameter.
class SetterVal a where
  setterVal :: a -> AudioParameter -> AudioParameter

instance setterValNumber :: SetterVal Number where
  setterVal = const <<< AudioParameter <<< defaultParam { param = _ }

instance setterValAudioParameter :: SetterVal AudioParameter where
  setterVal = const

instance setterValTuple :: SetterVal (Tuple a (AudioParameter -> AudioParameter)) where
  setterVal = snd

instance setterValTupleN :: SetterVal (Tuple a (AudioParameter -> Number)) where
  setterVal = map param <<< snd

instance setterValFunction :: SetterVal (AudioParameter -> AudioParameter) where
  setterVal = identity

instance setterValFunctionN :: SetterVal (AudioParameter -> Number) where
  setterVal = map param

-- | Internal class used to make term-level instructions for audio unit changes.
class
  AudioInterpret audio engine <= ChangeInstructions (audio :: Type) (engine :: Type) (g :: Type) (h :: Type) | g -> h where
  changeInstructions :: Partial => Int -> g -> AnAudioUnit -> h /\ Array (audio -> engine) /\ AnAudioUnit

instance changeInstructionsAllpass :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB) => ChangeInstructions audio engine (CTOR.Allpass argA argB argC) (CTOR.Allpass AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.Allpass argA argB _) (AAllpass v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setQ idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setGain idx argB_iv' ]
    in
      (CTOR.Allpass argA_iv' argB_iv' unit)
        /\ (argA_Changes <> argB_Changes)
        /\ AAllpass argA_iv' argB_iv'

instance changeInstructionsBandpass :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB) => ChangeInstructions audio engine (CTOR.Bandpass argA argB argC) (CTOR.Bandpass AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.Bandpass argA argB _) (ABandpass v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setFrequency idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setQ idx argB_iv' ]
    in
      (CTOR.Bandpass argA_iv' argB_iv' unit)
        /\ (argA_Changes <> argB_Changes)
        /\ ABandpass argA_iv' argB_iv'

instance changeInstructionsConstant :: (AudioInterpret audio engine, SetterVal argA) => ChangeInstructions audio engine (CTOR.Constant argA) (CTOR.Constant AudioParameter) where
  changeInstructions idx (CTOR.Constant onOff argA) (AConstant oldOnOff v_argA@(AudioParameter v_argA')) =
    let
      onOffDiff = oldOnOff /= onOff

      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setOffset idx argA_iv' ]
    in
      (CTOR.Constant onOff argA_iv')
        /\ (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) idx ] else []))
        /\ AConstant onOff argA_iv'

instance changeInstructionsConvolver :: AudioInterpret audio engine => ChangeInstructions audio engine (CTOR.Convolver argA argB) (CTOR.Convolver argA Unit) where
  changeInstructions _ (CTOR.Convolver a _) x = (CTOR.Convolver a unit) /\ empty /\ x

instance changeInstructionsDelay :: (AudioInterpret audio engine, SetterVal argA) => ChangeInstructions audio engine (CTOR.Delay argA argB) (CTOR.Delay AudioParameter Unit) where
  changeInstructions idx (CTOR.Delay argA _) (ADelay v_argA@(AudioParameter v_argA')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setDelay idx argA_iv' ]
    in
      (CTOR.Delay argA_iv' unit)
        /\ (argA_Changes)
        /\ ADelay argA_iv'

instance changeInstructionsDynamicsCompressor :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB, SetterVal argC, SetterVal argD, SetterVal argE) => ChangeInstructions audio engine (CTOR.DynamicsCompressor argA argB argC argD argE argF) (CTOR.DynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.DynamicsCompressor argA argB argC argD argE _) (ADynamicsCompressor v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') v_argC@(AudioParameter v_argC') v_argD@(AudioParameter v_argD') v_argE@(AudioParameter v_argE')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setThreshold idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setKnee idx argB_iv' ]

      s_argC = setterVal argC

      argC_iv' = s_argC v_argC

      argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param then [] else [ setRatio idx argC_iv' ]

      s_argD = setterVal argD

      argD_iv' = s_argD v_argD

      argD_Changes = let AudioParameter argD_iv = argD_iv' in if argD_iv.param == v_argD'.param then [] else [ setAttack idx argD_iv' ]

      s_argE = setterVal argE

      argE_iv' = s_argE v_argE

      argE_Changes = let AudioParameter argE_iv = argE_iv' in if argE_iv.param == v_argE'.param then [] else [ setRelease idx argE_iv' ]
    in
      (CTOR.DynamicsCompressor argA_iv' argB_iv' argC_iv' argD_iv' argE_iv' unit)
        /\ (argA_Changes <> argB_Changes <> argC_Changes <> argD_Changes <> argE_Changes)
        /\ ADynamicsCompressor argA_iv' argB_iv' argC_iv' argD_iv' argE_iv'

instance changeInstructionsGain :: (AudioInterpret audio engine, SetterVal argA) => ChangeInstructions audio engine (CTOR.Gain argA argB) (CTOR.Gain AudioParameter Unit) where
  changeInstructions idx (CTOR.Gain argA _) (AGain v_argA@(AudioParameter v_argA')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setGain idx argA_iv' ]
    in
      (CTOR.Gain argA_iv' unit)
        /\ (argA_Changes)
        /\ AGain argA_iv'

instance changeInstructionsHighpass :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB) => ChangeInstructions audio engine (CTOR.Highpass argA argB argC) (CTOR.Highpass AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.Highpass argA argB _) (AHighpass v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setFrequency idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setQ idx argB_iv' ]
    in
      (CTOR.Highpass argA_iv' argB_iv' unit)
        /\ (argA_Changes <> argB_Changes)
        /\ AHighpass argA_iv' argB_iv'

instance changeInstructionsHighshelf :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB) => ChangeInstructions audio engine (CTOR.Highshelf argA argB argC) (CTOR.Highshelf AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.Highshelf argA argB _) (AHighshelf v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setFrequency idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setGain idx argB_iv' ]
    in
      (CTOR.Highshelf argA_iv' argB_iv' unit)
        /\ (argA_Changes <> argB_Changes)
        /\ AHighshelf argA_iv' argB_iv'

instance changeInstructionsLoopBuf :: (AudioInterpret audio engine, SetterVal argB) => ChangeInstructions audio engine (CTOR.LoopBuf argB) (CTOR.LoopBuf AudioParameter) where
  changeInstructions idx (CTOR.LoopBuf bf onOff argB loopStart loopEnd) (ALoopBuf x oldOnOff v_argB@(AudioParameter v_argB') oldLoopStart oldLoopEnd) =
    let
      onOffDiff = oldOnOff /= onOff

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not (onOffDiff && onOff == On) then [] else [ setPlaybackRate idx argB_iv' ]
    in
      (CTOR.LoopBuf bf onOff argB_iv' loopStart loopEnd)
        /\ ((if bf /= x then [ setBuffer idx bf ] else []) <> argB_Changes <> (if (oldLoopStart /= loopStart) || (onOffDiff && onOff == On) then [ setLoopStart idx loopStart ] else []) <> (if (oldLoopEnd /= loopEnd) || (onOffDiff && onOff == On) then [ setLoopEnd idx loopEnd ] else []) <> (if onOffDiff then [ (if onOff == On then setOn else setOff) idx ] else []))
        /\ ALoopBuf bf onOff argB_iv' loopStart loopEnd

instance changeInstructionsLowpass :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB) => ChangeInstructions audio engine (CTOR.Lowpass argA argB argC) (CTOR.Lowpass AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.Lowpass argA argB _) (ALowpass v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setFrequency idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setQ idx argB_iv' ]
    in
      (CTOR.Lowpass argA_iv' argB_iv' unit)
        /\ (argA_Changes <> argB_Changes)
        /\ ALowpass argA_iv' argB_iv'

instance changeInstructionsLowshelf :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB) => ChangeInstructions audio engine (CTOR.Lowshelf argA argB argC) (CTOR.Lowshelf AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.Lowshelf argA argB _) (ALowshelf v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setFrequency idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setGain idx argB_iv' ]
    in
      (CTOR.Lowshelf argA_iv' argB_iv' unit)
        /\ (argA_Changes <> argB_Changes)
        /\ ALowshelf argA_iv' argB_iv'

instance changeInstructionsMicrophone :: AudioInterpret audio engine => ChangeInstructions audio engine CTOR.Microphone CTOR.Microphone where
  changeInstructions _ g x = g /\ empty /\ x

instance changeInstructionsNotch :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB) => ChangeInstructions audio engine (CTOR.Notch argA argB argC) (CTOR.Notch AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.Notch argA argB _) (ANotch v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setFrequency idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setQ idx argB_iv' ]
    in
      (CTOR.Notch argA_iv' argB_iv' unit)
        /\ (argA_Changes <> argB_Changes)
        /\ ANotch argA_iv' argB_iv'

instance changeInstructionsPeaking :: (AudioInterpret audio engine, SetterVal argA, SetterVal argB, SetterVal argC) => ChangeInstructions audio engine (CTOR.Peaking argA argB argC argD) (CTOR.Peaking AudioParameter AudioParameter AudioParameter Unit) where
  changeInstructions idx (CTOR.Peaking argA argB argC _) (APeaking v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') v_argC@(AudioParameter v_argC')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setFrequency idx argA_iv' ]

      s_argB = setterVal argB

      argB_iv' = s_argB v_argB

      argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ setQ idx argB_iv' ]

      s_argC = setterVal argC

      argC_iv' = s_argC v_argC

      argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param then [] else [ setGain idx argC_iv' ]
    in
      (CTOR.Peaking argA_iv' argB_iv' argC_iv' unit)
        /\ (argA_Changes <> argB_Changes <> argC_Changes)
        /\ APeaking argA_iv' argB_iv' argC_iv'

instance changeInstructionsPeriodicOsc :: (AudioInterpret audio engine, SetterVal argB) => ChangeInstructions audio engine (CTOR.PeriodicOsc argB) (CTOR.PeriodicOsc AudioParameter) where
  changeInstructions idx (CTOR.PeriodicOsc po onOff argB) = case _ of
    APeriodicOsc x oldOnOff v_argB@(AudioParameter v_argB') ->
      let
        onOffDiff = oldOnOff /= onOff

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not (onOffDiff && onOff == On) then [] else [ setFrequency idx argB_iv' ]
      in
        (CTOR.PeriodicOsc po onOff argB_iv')
          /\ ((if po /= x then [ setPeriodicOsc idx po ] else []) <> argB_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) idx ] else []))
          /\ APeriodicOsc po onOff argB_iv'

instance changeInstructionsPlayBuf :: (AudioInterpret audio engine, SetterVal argC) => ChangeInstructions audio engine (CTOR.PlayBuf argC) (CTOR.PlayBuf AudioParameter) where
  -- todo: set ny if different
  changeInstructions idx (CTOR.PlayBuf bf tx onOff argC) (APlayBuf x y oldOnOff v_argC@(AudioParameter v_argC')) =
    let
      onOffDiff = oldOnOff /= onOff

      s_argC = setterVal argC

      argC_iv' = s_argC v_argC

      argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param && not (onOffDiff && onOff == On) then [] else [ setPlaybackRate idx argC_iv' ]
    in
      (CTOR.PlayBuf bf tx onOff argC_iv')
        /\ ((if bf /= x then [ setBuffer idx bf ] else []) <> argC_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) idx ] else []))
        /\ APlayBuf bf y onOff argC_iv'

instance changeInstructionsRecorder :: AudioInterpret audio engine => ChangeInstructions audio engine (CTOR.Recorder argA argB) (CTOR.Recorder argA Unit) where
  changeInstructions _ (CTOR.Recorder sym _) x = (CTOR.Recorder sym unit) /\ empty /\ x

instance changeInstructionsSawtoothOsc :: (AudioInterpret audio engine, SetterVal argA) => ChangeInstructions audio engine (CTOR.SawtoothOsc argA) (CTOR.SawtoothOsc AudioParameter) where
  changeInstructions idx (CTOR.SawtoothOsc onOff argA) (ASawtoothOsc oldOnOff v_argA@(AudioParameter v_argA')) =
    let
      onOffDiff = oldOnOff /= onOff

      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not (onOffDiff && onOff == On) then [] else [ setFrequency idx argA_iv' ]
    in
      (CTOR.SawtoothOsc onOff argA_iv')
        /\ (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) idx ] else []))
        /\ ASawtoothOsc onOff argA_iv'

instance changeInstructionsSinOsc :: (AudioInterpret audio engine, SetterVal argA) => ChangeInstructions audio engine (CTOR.SinOsc argA) (CTOR.SinOsc AudioParameter) where
  changeInstructions idx (CTOR.SinOsc onOff argA) (ASinOsc oldOnOff v_argA@(AudioParameter v_argA')) =
    let
      onOffDiff = oldOnOff /= onOff

      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not (onOffDiff && onOff == On) then [] else [ setFrequency idx argA_iv' ]
    in
      (CTOR.SinOsc onOff argA_iv')
        /\ (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) idx ] else []))
        /\ ASinOsc onOff argA_iv'

instance changeInstructionsSpeaker :: AudioInterpret audio engine => ChangeInstructions audio engine (CTOR.Speaker argA) (CTOR.Speaker Unit) where
  changeInstructions _ _ x = (CTOR.Speaker unit) /\ empty /\ x

instance changeInstructionsSquareOsc :: (AudioInterpret audio engine, SetterVal argA) => ChangeInstructions audio engine (CTOR.SquareOsc argA) (CTOR.SquareOsc AudioParameter) where
  changeInstructions idx (CTOR.SquareOsc onOff argA) (ASquareOsc oldOnOff v_argA@(AudioParameter v_argA')) =
    let
      onOffDiff = oldOnOff /= onOff

      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not (onOffDiff && onOff == On) then [] else [ setFrequency idx argA_iv' ]
    in
      (CTOR.SquareOsc onOff argA_iv')
        /\ (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) idx ] else []))
        /\ ASquareOsc onOff argA_iv'

instance changeInstructionsStereoPanner :: (AudioInterpret audio engine, SetterVal argA) => ChangeInstructions audio engine (CTOR.StereoPanner argA argB) (CTOR.StereoPanner AudioParameter Unit) where
  changeInstructions idx (CTOR.StereoPanner argA _) (AStereoPanner v_argA@(AudioParameter v_argA')) =
    let
      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ setPan idx argA_iv' ]
    in
      CTOR.StereoPanner argA_iv' unit
        /\ (argA_Changes)
        /\ AStereoPanner argA_iv'

instance changeInstructionsTriangleOsc :: (AudioInterpret audio engine, SetterVal argA) => ChangeInstructions audio engine (CTOR.TriangleOsc argA) (CTOR.TriangleOsc AudioParameter) where
  changeInstructions idx orig@(CTOR.TriangleOsc onOff argA) (ATriangleOsc oldOnOff v_argA@(AudioParameter v_argA')) =
    let
      onOffDiff = oldOnOff /= onOff

      s_argA = setterVal argA

      argA_iv' = s_argA v_argA

      argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not (onOffDiff && onOff == On) then [] else [ setFrequency idx argA_iv' ]
    in
      CTOR.TriangleOsc onOff argA_iv'
        /\ (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) idx ] else []))
        /\ ATriangleOsc onOff argA_iv'

instance changeInstructionsWaveShaper :: AudioInterpret audio engine => ChangeInstructions audio engine (CTOR.WaveShaper argA argB argC) (CTOR.WaveShaper argA argB Unit) where
  changeInstructions _ (CTOR.WaveShaper argA argB _) x = (CTOR.WaveShaper argA argB unit) /\ empty /\ x

type ChangeType (p :: EdgeProfile) (a :: Type) (graph :: Graph) (o :: Type)
  = forall env audio engine proof m res ptr changeBit skolems. Monad m => AudioInterpret audio engine => Proxy p -> a -> FrameT env audio engine proof m res (UniverseC ptr graph changeBit skolems) (UniverseC ptr graph (Succ changeBit) skolems) o

type ChangesType (a :: Type) (g :: Graph) (o :: Type)
  = forall env audio engine proof m res ptr changeBit skolems. Monad m => AudioInterpret audio engine => a -> FrameT env audio engine proof m res (UniverseC ptr g changeBit skolems) (UniverseC ptr g (Succ changeBit) skolems) o

data ChangeInstruction a b
  = ChangeInstruction a b

instance changesUnit :: Changes Unit g Unit where
  changes _ = unsafeFrame (pure unit)
else instance changesPx :: Change p a graph b => Changes (ChangeInstruction (Proxy p) a) graph (ChangeInstruction (Proxy p) b) where
  changes (ChangeInstruction p a) = (change' :: ChangeType p a graph b) p a
else instance changesTp :: (Changes x graph x', Changes y graph y') => Changes (Tuple x y) graph (Tuple x' y') where
  changes (x /\ y) = unsafeFrame (Tuple <$> x' <*> y')
    where
    x' = unsafeUnframe $ (changes :: ChangesType x graph x') x

    y' = unsafeUnframe $ (changes :: ChangesType y graph y') y
else instance changesSingle :: (TerminalIdentityEdge graph edge, Change edge a graph b) => Changes a graph b where
  changes a = change a

-- | Internal helper class used for changing audio nodes.
class
  Change (SingleEdge p) a graph b <= ChangeP (p :: Ptr) (a :: Type) (graph :: Graph) (b :: Type) | p a graph -> b where
  changeP :: forall env audio engine proof m res ptr changeBit skolems. Monad m => AudioInterpret audio engine => Proxy p -> a -> FrameT env audio engine proof m res (UniverseC ptr graph changeBit skolems) (UniverseC ptr graph (Succ changeBit) skolems) b

instance changePAll :: Change (SingleEdge p) a graph b => ChangeP p a graph b where
  changeP _ = change' (Proxy :: _ (SingleEdge p))

-- | Internal helper class used for changing audio nodes.
class ModifyRes (tag :: Type) (p :: Ptr) (i :: Node) (mod :: NodeList) (plist :: EdgeProfile) | tag p i -> mod plist

instance modifyResAllpass :: ModifyRes (CTOR.Allpass a b c) ptr (NodeC (AU.TAllpass ptr) edge) (NodeListCons (NodeC (AU.TAllpass ptr) edge) NodeListNil) edge
else instance modifyResBandpass :: ModifyRes (CTOR.Bandpass a b c) ptr (NodeC (AU.TBandpass ptr) edge) (NodeListCons (NodeC (AU.TBandpass ptr) edge) NodeListNil) edge
else instance modifyResConstant :: ModifyRes (CTOR.Constant a) ptr (NodeC (AU.TConstant ptr) edge) (NodeListCons (NodeC (AU.TConstant ptr) edge) NodeListNil) edge
else instance modifyResConvolver :: ModifyRes (CTOR.Convolver a b) ptr (NodeC (AU.TConvolver ptr name) edge) (NodeListCons (NodeC (AU.TConvolver ptr name) edge) NodeListNil) edge
else instance modifyResDelay :: ModifyRes (CTOR.Delay a b) ptr (NodeC (AU.TDelay ptr) edge) (NodeListCons (NodeC (AU.TDelay ptr) edge) NodeListNil) edge
else instance modifyResDynamicsCompressor :: ModifyRes (CTOR.DynamicsCompressor a b c d e f) ptr (NodeC (AU.TDynamicsCompressor ptr) edge) (NodeListCons (NodeC (AU.TDynamicsCompressor ptr) edge) NodeListNil) edge
else instance modifyResGain :: ModifyRes (CTOR.Gain a b) ptr (NodeC (AU.TGain ptr) edge) (NodeListCons (NodeC (AU.TGain ptr) edge) NodeListNil) edge
else instance modifyResHighpass :: ModifyRes (CTOR.Highpass a b c) ptr (NodeC (AU.THighpass ptr) edge) (NodeListCons (NodeC (AU.THighpass ptr) edge) NodeListNil) edge
else instance modifyResHighshelf :: ModifyRes (CTOR.Highshelf a b c) ptr (NodeC (AU.THighshelf ptr) edge) (NodeListCons (NodeC (AU.THighshelf ptr) edge) NodeListNil) edge
else instance modifyResLoopBuf :: ModifyRes (CTOR.LoopBuf a) ptr (NodeC (AU.TLoopBuf ptr) edge) (NodeListCons (NodeC (AU.TLoopBuf ptr) edge) NodeListNil) edge
else instance modifyResLowpass :: ModifyRes (CTOR.Lowpass a b c) ptr (NodeC (AU.TLowpass ptr) edge) (NodeListCons (NodeC (AU.TLowpass ptr) edge) NodeListNil) edge
else instance modifyResLowshelf :: ModifyRes (CTOR.Lowshelf a b c) ptr (NodeC (AU.TLowshelf ptr) edge) (NodeListCons (NodeC (AU.TLowshelf ptr) edge) NodeListNil) edge
else instance modifyResMicrophone :: ModifyRes (CTOR.Microphone) ptr (NodeC (AU.TMicrophone ptr) edge) (NodeListCons (NodeC (AU.TMicrophone ptr) edge) NodeListNil) edge
else instance modifyResNotch :: ModifyRes (CTOR.Notch a b c) ptr (NodeC (AU.TNotch ptr) edge) (NodeListCons (NodeC (AU.TNotch ptr) edge) NodeListNil) edge
else instance modifyResPeaking :: ModifyRes (CTOR.Peaking a b c d) ptr (NodeC (AU.TPeaking ptr) edge) (NodeListCons (NodeC (AU.TPeaking ptr) edge) NodeListNil) edge
else instance modifyResPeriodicOsc :: ModifyRes (CTOR.PeriodicOsc a) ptr (NodeC (AU.TPeriodicOsc ptr) edge) (NodeListCons (NodeC (AU.TPeriodicOsc ptr) edge) NodeListNil) edge
else instance modifyResPlayBuf :: ModifyRes (CTOR.PlayBuf a) ptr (NodeC (AU.TPlayBuf ptr) edge) (NodeListCons (NodeC (AU.TPlayBuf ptr) edge) NodeListNil) edge
else instance modifyResRecorder :: ModifyRes (CTOR.Recorder a b) ptr (NodeC (AU.TRecorder ptr name) edge) (NodeListCons (NodeC (AU.TRecorder ptr name) edge) NodeListNil) edge
else instance modifyResSawtoothOsc :: ModifyRes (CTOR.SawtoothOsc a) ptr (NodeC (AU.TSawtoothOsc ptr) edge) (NodeListCons (NodeC (AU.TSawtoothOsc ptr) edge) NodeListNil) edge
else instance modifyResSinOsc :: ModifyRes (CTOR.SinOsc a) ptr (NodeC (AU.TSinOsc ptr) edge) (NodeListCons (NodeC (AU.TSinOsc ptr) edge) NodeListNil) edge
else instance modifyResSpeaker :: ModifyRes (CTOR.Speaker a) ptr (NodeC (AU.TSpeaker ptr) edge) (NodeListCons (NodeC (AU.TSpeaker ptr) edge) NodeListNil) edge
else instance modifyResSquareOsc :: ModifyRes (CTOR.SquareOsc a) ptr (NodeC (AU.TSquareOsc ptr) edge) (NodeListCons (NodeC (AU.TSquareOsc ptr) edge) NodeListNil) edge
else instance modifyResStereoPanner :: ModifyRes (CTOR.StereoPanner a b) ptr (NodeC (AU.TStereoPanner ptr) edge) (NodeListCons (NodeC (AU.TStereoPanner ptr) edge) NodeListNil) edge
else instance modifyResTriangleOsc :: ModifyRes (CTOR.TriangleOsc a) ptr (NodeC (AU.TTriangleOsc ptr) edge) (NodeListCons (NodeC (AU.TTriangleOsc ptr) edge) NodeListNil) edge
else instance modifyResWaveShaper :: ModifyRes (CTOR.WaveShaper a b c) ptr (NodeC (AU.TWaveShaper ptr name) edge) (NodeListCons (NodeC (AU.TWaveShaper ptr name) edge) NodeListNil) edge
else instance modifyResMiss :: ModifyRes tag p n NodeListNil NoEdge

-- | Internal helper class used for changing audio nodes.
class Modify' (tag :: Type) (p :: Ptr) (i :: NodeList) (mod :: NodeList) (nextP :: EdgeProfile) | tag p i -> mod nextP

instance modifyNil :: Modify' tag p NodeListNil NodeListNil NoEdge

instance modifyCons ::
  ( ModifyRes tag p head headResAsList headPlist
  , Modify' tag p tail tailResAsList tailPlist
  , NodeListAppend headResAsList tailResAsList o
  , AltEdgeProfile headPlist tailPlist plist
  ) =>
  Modify' tag p (NodeListCons head tail) o plist

-- | Internal helper class used for changing audio nodes.
class Modify (tag :: Type) (p :: Ptr) (i :: Graph) (nextP :: EdgeProfile) | tag p i -> nextP

instance modify ::
  ( GraphToNodeList ig il
  , Modify' tag p il (NodeListCons x NodeListNil) nextP
  ) =>
  Modify tag p ig nextP

changeAudioUnit ::
  forall g g' env audio engine proof m res currentIdx (igraph :: Graph) changeBit skolems (p :: Bits) (nextP :: EdgeProfile) univ.
  Partial =>
  Monad m =>
  AudioInterpret audio engine =>
  ChangeInstructions audio engine g g' =>
  BinToInt p =>
  Modify g p igraph nextP =>
  Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph) -> g -> FrameT env audio engine proof m res univ (UniverseC currentIdx igraph changeBit skolems) g'
changeAudioUnit _ g =
  unsafeFrame
    $ do
        let
          ptr = toInt' (Proxy :: _ p)
        (oo /\ instr /\ au) <- (fromJust <<< M.lookup ptr) <$> gets _.internalNodes
        modify_
          ( \i ->
              i
                { internalNodes = M.insert ptr (au) i.internalNodes
                , instructions = i.instructions <> instr
                }
          )
        pure oo

instance changeNoEdge ::
  Change NoEdge g igraph g where
  change' _ = unsafeFrame <<< pure

instance changeSkolem ::
  Change (SingleEdge p) (Proxy skolem) igraph (Proxy skolem) where
  change' _ = unsafeFrame <<< pure

instance changeIdentity :: Change (SingleEdge p) x igraph y => Change (SingleEdge p) (Identity x) igraph (Identity y) where
  change' p (Identity x) = change' p x

instance changeIgnoreMe :: Change (SingleEdge p) IgnoreMe igraph IgnoreMe where
  change' _ = unsafeFrame <<< pure

instance changeFocus :: Change (SingleEdge p) x igraph y => Change (SingleEdge p) (Focus x) igraph (Focus y) where
  change' p (Focus x) = change' p x

instance changeMany2 ::
  ( Change (SingleEdge p) x igraph x'
  , Change (ManyEdges a b) y igraph y'
  ) =>
  Change (ManyEdges p (PtrListCons a b)) (x /\ y) igraph (x' /\ y') where
  change' _ (x /\ y) = unsafeFrame (Tuple <$> _1 <*> _2)
    where
    _1 = unsafeUnframe $ (change' :: ChangeType (SingleEdge p) x igraph x') Proxy x

    _2 = unsafeUnframe $ (change' :: ChangeType (ManyEdges a b) y igraph y') Proxy y

instance changeMany1 ::
  Change (SingleEdge p) a igraph b =>
  Change (ManyEdges p PtrListNil) (a /\ Unit) igraph (b /\ Unit) where
  change' _ = Lens.over Lens._1 ((change' :: ChangeType (SingleEdge p) a igraph b) Proxy)

----------
instance changeDup ::
  ( Create
      a
      D0
      InitialGraph
      (SkolemListCons (SkolemPairC skolem D0) skolems)
      outptr
      graph
      (SkolemListCons (SkolemPairC skolem D0) skolems)
      ignore
  , BinToInt p
  , BinToInt outptr
  , BinToInt continuation
  , BinSub p outptr continuation
  , Change (SingleEdge p) b igraph b'
  , Change (SingleEdge continuation) a igraph a'
  ) =>
  Change (SingleEdge p) (CTOR.Dup a (Proxy skolem -> b)) igraph (CTOR.Dup a' b') where
  change' _ (CTOR.Dup a f) = unsafeFrame (CTOR.Dup <$> _a <*> _b)
    where
    _b = unsafeUnframe $ (change' :: ChangeType (SingleEdge p) b igraph b') Proxy (f Proxy)

    _a = unsafeUnframe $ (change' :: ChangeType (SingleEdge continuation) a igraph a') Proxy a

----------------------------------------
-----------------------------------
-------------
instance changeAllpass ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Allpass argA argB argC) p igraph nextP
  , Change nextP argC igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Allpass argA argB fOfargC) igraph (CTOR.Allpass AudioParameter AudioParameter outInner) where
  change' _ (CTOR.Allpass argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      WAGS.do
        (CTOR.Allpass argA' argB' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Allpass argA argB argC)
        CTOR.Allpass argA' argB' <$> (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeBandpass ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Bandpass argA argB argC) p igraph nextP
  , Change nextP argC igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Bandpass argA argB fOfargC) igraph (CTOR.Bandpass AudioParameter AudioParameter outInner) where
  change' _ (CTOR.Bandpass argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      WAGS.do
        (CTOR.Bandpass argA' argB' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Bandpass argA argB argC)
        CTOR.Bandpass argA' argB' <$> (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeConstant ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.Constant argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.Constant argA) igraph (CTOR.Constant AudioParameter) where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeDelay ::
  ( SetterVal argA
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , Modify (CTOR.Delay argA argB) p igraph nextP
  , Change nextP argB igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Delay argA fOfargB) igraph (CTOR.Delay AudioParameter outInner) where
  change' _ (CTOR.Delay argA fOfargB) =
    let
      argB = (((toSkolemizedFunction :: fOfargB -> (Proxy skolem -> argB)) fOfargB) Proxy)
    in
      WAGS.do
        (CTOR.Delay argA' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Delay argA argB)
        CTOR.Delay argA' <$> (change' :: ChangeType nextP argB igraph) Proxy argB

instance changeDynamicsCompressor ::
  ( SetterVal argA
  , SetterVal argB
  , SetterVal argC
  , SetterVal argD
  , SetterVal argE
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargF skolem
  , ToSkolemizedFunction fOfargF skolem argF
  , Modify (CTOR.DynamicsCompressor argA argB argC argD argE argF) p igraph nextP
  , Change nextP argF igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.DynamicsCompressor argA argB argC argD argE fOfargF) igraph (CTOR.DynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter outInner) where
  change' _ (CTOR.DynamicsCompressor argA argB argC argD argE fOfargF) =
    let
      argF = (((toSkolemizedFunction :: fOfargF -> (Proxy skolem -> argF)) fOfargF) Proxy)
    in
      WAGS.do
        (CTOR.DynamicsCompressor argA' argB' argC' argD' argE' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.DynamicsCompressor argA argB argC argD argE argF)
        CTOR.DynamicsCompressor argA' argB' argC' argD' argE' <$> (change' :: ChangeType nextP argF igraph) Proxy argF

instance changeHighpass ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Highpass argA argB argC) p igraph nextP
  , Change nextP argC igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Highpass argA argB fOfargC) igraph (CTOR.Highpass AudioParameter AudioParameter outInner) where
  change' _ (CTOR.Highpass argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      WAGS.do
        (CTOR.Highpass argA' argB' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Highpass argA argB argC)
        CTOR.Highpass argA' argB' <$> (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeHighshelf ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Highshelf argA argB argC) p igraph nextP
  , Change nextP argC igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Highshelf argA argB fOfargC) igraph (CTOR.Highshelf AudioParameter AudioParameter outInner) where
  change' _ (CTOR.Highshelf argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      WAGS.do
        (CTOR.Highpass argA' argB' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Highshelf argA argB argC)
        CTOR.Highpass argA' argB' <$> (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeLoopBuf ::
  ( SetterVal argB
  , BinToInt p
  , Modify (CTOR.LoopBuf argB) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.LoopBuf argB) igraph (CTOR.LoopBuf AudioParameter) where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeLowpass ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Lowpass argA argB argC) p igraph nextP
  , Change nextP argC igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Lowpass argA argB fOfargC) igraph (CTOR.Lowpass AudioParameter AudioParameter outInner) where
  change' _ (CTOR.Lowpass argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      WAGS.do
        (CTOR.Lowpass argA' argB' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Lowpass argA argB argC)
        CTOR.Lowpass argA' argB' <$> (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeLowshelf ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Lowshelf argA argB argC) p igraph nextP
  , Change nextP argC igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Lowshelf argA argB fOfargC) igraph (CTOR.Lowshelf AudioParameter AudioParameter outInner) where
  change' _ (CTOR.Lowshelf argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      WAGS.do
        (CTOR.Lowshelf argA' argB' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Lowshelf argA argB argC)
        CTOR.Lowshelf argA' argB' <$> (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeMicrophone ::
  Change (SingleEdge p) (CTOR.Microphone) igraph CTOR.Microphone where
  change' _ = unsafeFrame <<< pure

instance changeNotch ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Notch argA argB argC) p igraph nextP
  , Change nextP argC igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Notch argA argB fOfargC) igraph (CTOR.Notch AudioParameter AudioParameter outInner) where
  change' _ (CTOR.Notch argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      WAGS.do
        (CTOR.Notch argA' argB' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Notch argA argB argC)
        CTOR.Notch argA' argB' <$> (change' :: ChangeType nextP argC igraph) Proxy argC

instance changePeaking ::
  ( SetterVal argA
  , SetterVal argB
  , SetterVal argC
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargD skolem
  , ToSkolemizedFunction fOfargD skolem argD
  , Modify (CTOR.Peaking argA argB argC argD) p igraph nextP
  , Change nextP argD igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Peaking argA argB argC fOfargD) igraph (CTOR.Peaking AudioParameter AudioParameter AudioParameter outInner) where
  change' _ (CTOR.Peaking argA argB argC fOfargD) =
    let
      argD = (((toSkolemizedFunction :: fOfargD -> (Proxy skolem -> argD)) fOfargD) Proxy)
    in
      WAGS.do
        (CTOR.Peaking argA' argB' argC' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Peaking argA argB argC argD)
        CTOR.Peaking argA' argB' argC' <$> (change' :: ChangeType nextP argD igraph) Proxy argD

instance changePeriodicOsc ::
  ( SetterVal argB
  , BinToInt p
  , Modify (CTOR.PeriodicOsc argB) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.PeriodicOsc argB) igraph (CTOR.PeriodicOsc AudioParameter) where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changePlayBuf ::
  ( SetterVal argB
  , BinToInt p
  , Modify (CTOR.PlayBuf argB) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.PlayBuf argB) igraph (CTOR.PlayBuf AudioParameter) where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeSawtoothOsc ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.SawtoothOsc argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.SawtoothOsc argA) igraph (CTOR.SawtoothOsc AudioParameter) where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeSinOsc ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.SinOsc argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.SinOsc argA) igraph (CTOR.SinOsc AudioParameter) where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeSquareOsc ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.SquareOsc argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.SquareOsc argA) igraph (CTOR.SquareOsc AudioParameter) where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeStereoPanner ::
  ( SetterVal argA
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , Modify (CTOR.StereoPanner argA argB) p igraph nextP
  , Change nextP argB igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.StereoPanner argA fOfargB) igraph (CTOR.StereoPanner AudioParameter outInner) where
  change' _ (CTOR.StereoPanner argA fOfargB) =
    let
      argB = (((toSkolemizedFunction :: fOfargB -> (Proxy skolem -> argB)) fOfargB) Proxy)
    in
      WAGS.do
        (CTOR.StereoPanner argA' _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.StereoPanner argA argB)
        CTOR.StereoPanner argA' <$> (change' :: ChangeType nextP argB igraph) Proxy argB

instance changeTriangleOsc ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.TriangleOsc argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.TriangleOsc argA) igraph (CTOR.TriangleOsc AudioParameter) where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

--------------
instance changeGain ::
  ( SetterVal a
  , BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , Modify (CTOR.Gain a b) p igraph nextP
  , Change nextP b igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Gain a fb) igraph (CTOR.Gain AudioParameter outInner) where
  change' _ (CTOR.Gain a fb) =
    let
      b = (((toSkolemizedFunction :: fb -> (Proxy skolem -> b)) fb) Proxy)
    in
      WAGS.do
        (CTOR.Gain vol _) <- changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Gain a b)
        CTOR.Gain vol <$> (change' :: ChangeType nextP b igraph) Proxy b

instance changeSpeaker ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fa skolem
  , ToSkolemizedFunction fa skolem a
  , Modify (CTOR.Speaker a) p igraph nextP
  , Change nextP a igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Speaker fa) igraph (CTOR.Speaker outInner) where
  change' _ (CTOR.Speaker fa) =
    let
      a = (((toSkolemizedFunction :: fa -> (Proxy skolem -> a)) fa) Proxy)
    in
      WAGS.do
        ivoid $ changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Speaker a)
        CTOR.Speaker <$> (change' :: ChangeType nextP a igraph) Proxy a

instance changeWaveShaper ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , Modify (CTOR.WaveShaper sym overshape argB) p igraph nextP
  , Change nextP argB igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.WaveShaper sym overshape fOfargB) igraph (CTOR.WaveShaper sym overshape outInner) where
  change' _ (CTOR.WaveShaper _ _ fOfargB) =
    let
      argB = (((toSkolemizedFunction :: fOfargB -> (Proxy skolem -> argB)) fOfargB) Proxy)
    in
      (change' :: ChangeType nextP argB igraph) Proxy argB

instance changeRecorder ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fa skolem
  , ToSkolemizedFunction fa skolem a
  , Modify (CTOR.Recorder sym a) p igraph nextP
  , Change nextP a igraph outInner
  ) =>
  Change (SingleEdge p) (CTOR.Recorder sym fa) igraph (CTOR.Recorder sym outInner) where
  change' _ (CTOR.Recorder _ fa) =
    let
      a = (((toSkolemizedFunction :: fa -> (Proxy skolem -> a)) fa) Proxy)
    in
      (change' :: ChangeType nextP a igraph) Proxy a
