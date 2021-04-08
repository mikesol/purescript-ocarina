module WAGS.Change where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.State (gets, modify_)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Peano (Succ)
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (FrameT(..))
import WAGS.Create (class Create)
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Constructors (Dup(..), Gain(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..))
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam, param)
import WAGS.Rendered (AnAudioUnit(..), Instruction(..))
import WAGS.Universe.AudioUnit (AudioUnitRef)
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinSub, class BinToInt, BinL, D0, Ptr, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, Graph, InitialGraph)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class ToSkolemizedFunction, SkolemListCons, SkolemPairC, toSkolemizedFunction)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Validation (class AssertSingleton, class EdgeProfileChooseGreater, class NodeListAppend, class TerminalIdentityEdge)

class ChangeInstructions (g :: Type) where
  changeInstructions :: Int -> g -> AnAudioUnit -> Maybe (Array Instruction /\ AnAudioUnit)

--------------
--------
instance changeInstructionsAllpass :: (SetterVal argA, SetterVal argB) => ChangeInstructions (CTOR.Allpass argA argB argC) where
  changeInstructions idx (CTOR.Allpass argA argB _) = case _ of
    AAllpass v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetQ idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetGain idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes)
          /\ AAllpass argA_iv' argB_iv'
    _ -> Nothing

instance changeInstructionsBandpass :: (SetterVal argA, SetterVal argB) => ChangeInstructions (CTOR.Bandpass argA argB argC) where
  changeInstructions idx (CTOR.Bandpass argA argB _) = case _ of
    ABandpass v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes)
          /\ ABandpass argA_iv' argB_iv'
    _ -> Nothing

instance changeInstructionsConstant :: (SetterVal argA) => ChangeInstructions (CTOR.Constant argA) where
  changeInstructions idx (CTOR.Constant argA) = case _ of
    AConstant v_argA@(AudioParameter v_argA') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetOffset idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
      in
        Just
          $ (argA_Changes)
          /\ AConstant argA_iv'
    _ -> Nothing

instance changeInstructionsConvolver :: ChangeInstructions (CTOR.Convolver argA argB) where
  changeInstructions _ _ _ = Nothing

instance changeInstructionsDelay :: (SetterVal argA) => ChangeInstructions (CTOR.Delay argA argB) where
  changeInstructions idx (CTOR.Delay argA _) = case _ of
    ADelay v_argA@(AudioParameter v_argA') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetDelay idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
      in
        Just
          $ (argA_Changes)
          /\ ADelay argA_iv'
    _ -> Nothing

instance changeInstructionsDynamicsCompressor :: (SetterVal argA, SetterVal argB, SetterVal argC, SetterVal argD, SetterVal argE) => ChangeInstructions (CTOR.DynamicsCompressor argA argB argC argD argE argF) where
  changeInstructions idx (CTOR.DynamicsCompressor argA argB argC argD argE _) = case _ of
    ADynamicsCompressor v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') v_argC@(AudioParameter v_argC') v_argD@(AudioParameter v_argD') v_argE@(AudioParameter v_argE') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetThreshold idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetKnee idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]

        s_argC = setterVal argC

        argC_iv' = s_argC v_argC

        argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param then [] else [ SetRatio idx argC_iv.param argC_iv.timeOffset argC_iv.transition ]

        s_argD = setterVal argD

        argD_iv' = s_argD v_argD

        argD_Changes = let AudioParameter argD_iv = argD_iv' in if argD_iv.param == v_argD'.param then [] else [ SetAttack idx argD_iv.param argD_iv.timeOffset argD_iv.transition ]

        s_argE = setterVal argE

        argE_iv' = s_argE v_argE

        argE_Changes = let AudioParameter argE_iv = argE_iv' in if argE_iv.param == v_argE'.param then [] else [ SetRelease idx argE_iv.param argE_iv.timeOffset argE_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes <> argC_Changes <> argD_Changes <> argE_Changes)
          /\ ADynamicsCompressor argA_iv' argB_iv' argC_iv' argD_iv' argE_iv'
    _ -> Nothing

instance changeInstructionsGain :: (SetterVal argA) => ChangeInstructions (CTOR.Gain argA argB) where
  changeInstructions idx (CTOR.Gain argA _) = case _ of
    AGain v_argA@(AudioParameter v_argA') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetGain idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
      in
        Just
          $ (argA_Changes)
          /\ AGain argA_iv'
    _ -> Nothing

instance changeInstructionsHighpass :: (SetterVal argA, SetterVal argB) => ChangeInstructions (CTOR.Highpass argA argB argC) where
  changeInstructions idx (CTOR.Highpass argA argB _) = case _ of
    AHighpass v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes)
          /\ AHighpass argA_iv' argB_iv'
    _ -> Nothing

instance changeInstructionsHighshelf :: (SetterVal argA, SetterVal argB) => ChangeInstructions (CTOR.Highshelf argA argB argC) where
  changeInstructions idx (CTOR.Highshelf argA argB _) = case _ of
    AHighshelf v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetGain idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes)
          /\ AHighshelf argA_iv' argB_iv'
    _ -> Nothing

instance changeInstructionsLoopBuf :: (SetterVal argB) => ChangeInstructions (CTOR.LoopBuf argA argB) where
  changeInstructions idx (CTOR.LoopBuf _ argB _ _) = case _ of
    ALoopBuf x v_argB@(AudioParameter v_argB') y z ->
      let
        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetPlaybackRate idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argB_Changes)
          /\ ALoopBuf x argB_iv' y z
    _ -> Nothing

instance changeInstructionsLowpass :: (SetterVal argA, SetterVal argB) => ChangeInstructions (CTOR.Lowpass argA argB argC) where
  changeInstructions idx (CTOR.Lowpass argA argB _) = case _ of
    ALowpass v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes)
          /\ ALowpass argA_iv' argB_iv'
    _ -> Nothing

instance changeInstructionsLowshelf :: (SetterVal argA, SetterVal argB) => ChangeInstructions (CTOR.Lowshelf argA argB argC) where
  changeInstructions idx (CTOR.Lowshelf argA argB _) = case _ of
    ALowshelf v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetGain idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes)
          /\ ALowshelf argA_iv' argB_iv'
    _ -> Nothing

instance changeInstructionsMicrophone :: ChangeInstructions (CTOR.Microphone) where
  changeInstructions _ _ _ = Nothing

instance changeInstructionsNotch :: (SetterVal argA, SetterVal argB) => ChangeInstructions (CTOR.Notch argA argB argC) where
  changeInstructions idx (CTOR.Notch argA argB _) = case _ of
    ANotch v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes)
          /\ ANotch argA_iv' argB_iv'
    _ -> Nothing

instance changeInstructionsPeaking :: (SetterVal argA, SetterVal argB, SetterVal argC) => ChangeInstructions (CTOR.Peaking argA argB argC argD) where
  changeInstructions idx (CTOR.Peaking argA argB argC _) = case _ of
    APeaking v_argA@(AudioParameter v_argA') v_argB@(AudioParameter v_argB') v_argC@(AudioParameter v_argC') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]

        s_argC = setterVal argC

        argC_iv' = s_argC v_argC

        argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param then [] else [ SetGain idx argC_iv.param argC_iv.timeOffset argC_iv.transition ]
      in
        Just
          $ (argA_Changes <> argB_Changes <> argC_Changes)
          /\ APeaking argA_iv' argB_iv' argC_iv'
    _ -> Nothing

instance changeInstructionsPeriodicOsc :: (SetterVal argB) => ChangeInstructions (CTOR.PeriodicOsc argA argB) where
  changeInstructions idx (CTOR.PeriodicOsc _ argB) = case _ of
    APeriodicOsc x v_argB@(AudioParameter v_argB') ->
      let
        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetFrequency idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argB_Changes)
          /\ APeriodicOsc x argB_iv'
    _ -> Nothing

instance changeInstructionsPlayBuf :: (SetterVal argC) => ChangeInstructions (CTOR.PlayBuf argA argC) where
  changeInstructions idx (CTOR.PlayBuf _ _ argC) = case _ of
    APlayBuf x y v_argC@(AudioParameter v_argC') ->
      let
        s_argC = setterVal argC

        argC_iv' = s_argC v_argC

        argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param then [] else [ SetPlaybackRate idx argC_iv.param argC_iv.timeOffset argC_iv.transition ]
      in
        Just
          $ (argC_Changes)
          /\ APlayBuf x y argC_iv'
    _ -> Nothing

instance changeInstructionsRecorder :: ChangeInstructions (CTOR.Recorder argA argB) where
  changeInstructions _ _ _ = Nothing

instance changeInstructionsSawtoothOsc :: (SetterVal argA) => ChangeInstructions (CTOR.SawtoothOsc argA) where
  changeInstructions idx (CTOR.SawtoothOsc argA) = case _ of
    ASawtoothOsc v_argA@(AudioParameter v_argA') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
      in
        Just
          $ (argA_Changes)
          /\ ASawtoothOsc argA_iv'
    _ -> Nothing

instance changeInstructionsSinOsc :: (SetterVal argA) => ChangeInstructions (CTOR.SinOsc argA) where
  changeInstructions idx (CTOR.SinOsc argA) = case _ of
    ASinOsc v_argA@(AudioParameter v_argA') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
      in
        Just
          $ (argA_Changes)
          /\ ASinOsc argA_iv'
    _ -> Nothing

instance changeInstructionsSpeaker :: ChangeInstructions (CTOR.Speaker argA) where
  changeInstructions _ _ _ = Nothing

instance changeInstructionsSquareOsc :: (SetterVal argA) => ChangeInstructions (CTOR.SquareOsc argA) where
  changeInstructions idx (CTOR.SquareOsc argA) = case _ of
    ASquareOsc v_argA@(AudioParameter v_argA') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
      in
        Just
          $ (argA_Changes)
          /\ ASquareOsc argA_iv'
    _ -> Nothing

instance changeInstructionsStereoPanner :: (SetterVal argA) => ChangeInstructions (CTOR.StereoPanner argA argB) where
  changeInstructions idx (CTOR.StereoPanner argA _) = case _ of
    AStereoPanner v_argA@(AudioParameter v_argA') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetPan idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
      in
        Just
          $ (argA_Changes)
          /\ AStereoPanner argA_iv'
    _ -> Nothing

instance changeInstructionsTriangleOsc :: (SetterVal argA) => ChangeInstructions (CTOR.TriangleOsc argA) where
  changeInstructions idx (CTOR.TriangleOsc argA) = case _ of
    ATriangleOsc v_argA@(AudioParameter v_argA') ->
      let
        s_argA = setterVal argA

        argA_iv' = s_argA v_argA

        argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param then [] else [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
      in
        Just
          $ (argA_Changes)
          /\ ATriangleOsc argA_iv'
    _ -> Nothing

instance changeInstructionsWaveShaper :: ChangeInstructions (CTOR.WaveShaper argA argB argC) where
  changeInstructions _ _ _ = Nothing

-------
------------------

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

change ::
  forall edge m a currentIdx graph changeBit skolems env proof.
  Monad m =>
  TerminalIdentityEdge graph edge =>
  Change edge a graph =>
  a -> FrameT env proof m (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx graph (Succ changeBit) skolems) Unit
change = change' (Proxy :: _ edge)

changeAt ::
  forall ptr a env proof m currentIdx graph changeBit skolems.
  Monad m =>
  Change (SingleEdge ptr) a graph =>
  AudioUnitRef ptr -> a -> FrameT env proof m (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx graph (Succ changeBit) skolems) Unit
changeAt _ = change' (Proxy :: _ (SingleEdge ptr))

type ChangeType (p :: EdgeProfile) (a :: Type) (grapho :: Graph)
  = forall env proof m ptr changeBit skolems. Monad m => Proxy p -> a -> FrameT env proof m (UniverseC ptr grapho changeBit skolems) (UniverseC ptr grapho (Succ changeBit) skolems) Unit

type ChangesType (a :: Type) (g :: Graph)
  = forall env proof m ptr changeBit skolems. Monad m => a -> FrameT env proof m (UniverseC ptr g changeBit skolems) (UniverseC ptr g (Succ changeBit) skolems) Unit

class Changes (a :: Type) (g :: Graph) where
  changes :: forall env proof m ptr changeBit skolems. Monad m => a -> FrameT env proof m (UniverseC ptr g changeBit skolems) (UniverseC ptr g (Succ changeBit) skolems) Unit

data ChangeInstruction a b
  = ChangeInstruction a b

instance changesUnit :: Changes Unit g where
  changes _ = FrameT (pure unit)
else instance changesPx :: Change p a graph => Changes (ChangeInstruction (Proxy p) a) graph where
  changes (ChangeInstruction p a) = (change' :: ChangeType p a graph) p a
else instance changesTp :: (Changes x graph, Changes y graph) => Changes (Tuple x y) graph where
  changes (x /\ y) = FrameT (x' *> y')
    where
    FrameT x' = (changes :: ChangesType x graph) x

    FrameT y' = (changes :: ChangesType y graph) y
else instance changesSingle :: (TerminalIdentityEdge graph edge, Change edge a graph) => Changes a graph where
  changes a = change a

class Change (p :: EdgeProfile) (a :: Type) (grapho :: Graph) where
  change' :: forall env proof m ptr changeBit skolems. Monad m => Proxy p -> a -> FrameT env proof m (UniverseC ptr grapho changeBit skolems) (UniverseC ptr grapho (Succ changeBit) skolems) Unit

class ModifyRes (tag :: Type) (p :: Ptr) (i :: Node) (mod :: NodeList) (plist :: EdgeProfile) | tag p i -> mod plist

instance modifyResAllpass :: ModifyRes (CTOR.Allpass a b c) ptr (NodeC (AU.TAllpass ptr) edge) (NodeListCons (NodeC (AU.TAllpass ptr) edge) NodeListNil) edge
else instance modifyResBandpass :: ModifyRes (CTOR.Bandpass a b c) ptr (NodeC (AU.TBandpass ptr) edge) (NodeListCons (NodeC (AU.TBandpass ptr) edge) NodeListNil) edge
else instance modifyResConstant :: ModifyRes (CTOR.Constant a) ptr (NodeC (AU.TConstant ptr) edge) (NodeListCons (NodeC (AU.TConstant ptr) edge) NodeListNil) edge
else instance modifyResConvolver :: ModifyRes (CTOR.Convolver a b) ptr (NodeC (AU.TConvolver ptr) edge) (NodeListCons (NodeC (AU.TConvolver ptr) edge) NodeListNil) edge
else instance modifyResDelay :: ModifyRes (CTOR.Delay a b) ptr (NodeC (AU.TDelay ptr) edge) (NodeListCons (NodeC (AU.TDelay ptr) edge) NodeListNil) edge
else instance modifyResDynamicsCompressor :: ModifyRes (CTOR.DynamicsCompressor a b c d e f) ptr (NodeC (AU.TDynamicsCompressor ptr) edge) (NodeListCons (NodeC (AU.TDynamicsCompressor ptr) edge) NodeListNil) edge
else instance modifyResGain :: ModifyRes (CTOR.Gain a b) ptr (NodeC (AU.TGain ptr) edge) (NodeListCons (NodeC (AU.TGain ptr) edge) NodeListNil) edge
else instance modifyResHighpass :: ModifyRes (CTOR.Highpass a b c) ptr (NodeC (AU.THighpass ptr) edge) (NodeListCons (NodeC (AU.THighpass ptr) edge) NodeListNil) edge
else instance modifyResHighshelf :: ModifyRes (CTOR.Highshelf a b c) ptr (NodeC (AU.THighshelf ptr) edge) (NodeListCons (NodeC (AU.THighshelf ptr) edge) NodeListNil) edge
else instance modifyResLoopBuf :: ModifyRes (CTOR.LoopBuf a b) ptr (NodeC (AU.TLoopBuf ptr) edge) (NodeListCons (NodeC (AU.TLoopBuf ptr) edge) NodeListNil) edge
else instance modifyResLowpass :: ModifyRes (CTOR.Lowpass a b c) ptr (NodeC (AU.TLowpass ptr) edge) (NodeListCons (NodeC (AU.TLowpass ptr) edge) NodeListNil) edge
else instance modifyResLowshelf :: ModifyRes (CTOR.Lowshelf a b c) ptr (NodeC (AU.TLowshelf ptr) edge) (NodeListCons (NodeC (AU.TLowshelf ptr) edge) NodeListNil) edge
else instance modifyResMicrophone :: ModifyRes (CTOR.Microphone) ptr (NodeC (AU.TMicrophone ptr) edge) (NodeListCons (NodeC (AU.TMicrophone ptr) edge) NodeListNil) edge
else instance modifyResNotch :: ModifyRes (CTOR.Notch a b c) ptr (NodeC (AU.TNotch ptr) edge) (NodeListCons (NodeC (AU.TNotch ptr) edge) NodeListNil) edge
else instance modifyResPeaking :: ModifyRes (CTOR.Peaking a b c d) ptr (NodeC (AU.TPeaking ptr) edge) (NodeListCons (NodeC (AU.TPeaking ptr) edge) NodeListNil) edge
else instance modifyResPeriodicOsc :: ModifyRes (CTOR.PeriodicOsc a b) ptr (NodeC (AU.TPeriodicOsc ptr) edge) (NodeListCons (NodeC (AU.TPeriodicOsc ptr) edge) NodeListNil) edge
else instance modifyResPlayBuf :: ModifyRes (CTOR.PlayBuf a b) ptr (NodeC (AU.TPlayBuf ptr) edge) (NodeListCons (NodeC (AU.TPlayBuf ptr) edge) NodeListNil) edge
else instance modifyResRecorder :: ModifyRes (CTOR.Recorder a b) ptr (NodeC (AU.TRecorder ptr) edge) (NodeListCons (NodeC (AU.TRecorder ptr) edge) NodeListNil) edge
else instance modifyResSawtoothOsc :: ModifyRes (CTOR.SawtoothOsc a) ptr (NodeC (AU.TSawtoothOsc ptr) edge) (NodeListCons (NodeC (AU.TSawtoothOsc ptr) edge) NodeListNil) edge
else instance modifyResSinOsc :: ModifyRes (CTOR.SinOsc a) ptr (NodeC (AU.TSinOsc ptr) edge) (NodeListCons (NodeC (AU.TSinOsc ptr) edge) NodeListNil) edge
else instance modifyResSpeaker :: ModifyRes (CTOR.Speaker a) ptr (NodeC (AU.TSpeaker ptr) edge) (NodeListCons (NodeC (AU.TSpeaker ptr) edge) NodeListNil) edge
else instance modifyResSquareOsc :: ModifyRes (CTOR.SquareOsc a) ptr (NodeC (AU.TSquareOsc ptr) edge) (NodeListCons (NodeC (AU.TSquareOsc ptr) edge) NodeListNil) edge
else instance modifyResStereoPanner :: ModifyRes (CTOR.StereoPanner a b) ptr (NodeC (AU.TStereoPanner ptr) edge) (NodeListCons (NodeC (AU.TStereoPanner ptr) edge) NodeListNil) edge
else instance modifyResTriangleOsc :: ModifyRes (CTOR.TriangleOsc a) ptr (NodeC (AU.TTriangleOsc ptr) edge) (NodeListCons (NodeC (AU.TTriangleOsc ptr) edge) NodeListNil) edge
else instance modifyResWaveShaper :: ModifyRes (CTOR.WaveShaper a b c) ptr (NodeC (AU.TWaveShaper ptr) edge) (NodeListCons (NodeC (AU.TWaveShaper ptr) edge) NodeListNil) edge
else instance modifyResMiss :: ModifyRes tag p n NodeListNil NoEdge

class Modify' (tag :: Type) (p :: Ptr) (i :: NodeList) (mod :: NodeList) (nextP :: EdgeProfile) | tag p i -> mod nextP

instance modifyNil :: Modify' tag p NodeListNil NodeListNil NoEdge

instance modifyCons ::
  ( ModifyRes tag p head headResAsList headPlist
  , Modify' tag p tail tailResAsList tailPlist
  , NodeListAppend headResAsList tailResAsList o
  , EdgeProfileChooseGreater headPlist tailPlist plist
  ) =>
  Modify' tag p (NodeListCons head tail) o plist

class Modify (tag :: Type) (p :: Ptr) (i :: Graph) (nextP :: EdgeProfile) | tag p i -> nextP

instance modify ::
  ( GraphToNodeList ig il
  , Modify' tag p il mod nextP
  , AssertSingleton mod x
  ) =>
  Modify tag p ig nextP

changeAudioUnit ::
  forall g env proof m currentIdx (igraph :: Graph) changeBit skolems (p :: BinL) (nextP :: EdgeProfile) univ.
  Monad m =>
  ChangeInstructions g =>
  BinToInt p =>
  Modify g p igraph nextP =>
  Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph) -> g -> FrameT env proof m univ (UniverseC currentIdx igraph changeBit skolems) Unit
changeAudioUnit _ g =
  FrameT
    $ do
        let
          ptr = toInt' (Proxy :: _ p)
        anAudioUnit' <- M.lookup ptr <$> gets _.internalNodes
        case anAudioUnit' of
          Just (anAudioUnit) -> case changeInstructions ptr g anAudioUnit of
            Just (instr /\ au) ->
              modify_
                ( \i ->
                    i
                      { internalNodes = M.insert ptr (au) i.internalNodes
                      , instructions = i.instructions <> instr
                      }
                )
            Nothing -> pure unit
          Nothing -> pure unit

instance changeNoEdge ::
  Change NoEdge g igraph where
  change' _ _ = FrameT $ (pure unit)

instance changeSkolem ::
  Change (SingleEdge p) (Proxy skolem) igraph where
  change' _ _ = FrameT $ (pure unit)

instance changeIdentity :: Change (SingleEdge p) x igraph => Change (SingleEdge p) (Identity x) igraph where
  change' p (Identity x) = change' p x

instance changeFocus :: Change (SingleEdge p) x igraph => Change (SingleEdge p) (Focus x) igraph where
  change' p (Focus x) = change' p x

instance changeMany2 ::
  ( Change (SingleEdge p) x igraph
  , Change (ManyEdges a b) y igraph
  ) =>
  Change (ManyEdges p (PtrListCons a b)) (x /\ y) igraph where
  change' _ (x /\ y) = FrameT (_1 *> _2)
    where
    FrameT _1 = (change' :: ChangeType (SingleEdge p) x igraph) Proxy x

    FrameT _2 = (change' :: ChangeType (ManyEdges a b) y igraph) Proxy y

instance changeMany1 ::
  Change (SingleEdge p) a igraph =>
  Change (ManyEdges p PtrListNil) (a /\ Unit) igraph where
  change' _ (a /\ _) = (change' :: ChangeType (SingleEdge p) a igraph) Proxy a

----------
instance changeDup ::
  ( Create
      a
      (UniverseC D0 InitialGraph changeBit (SkolemListCons (SkolemPairC skolem D0) skolems))
      (UniverseC outptr grapho changeBit (SkolemListCons (SkolemPairC skolem D0) skolems))
      ignore
  , BinToInt p
  , BinToInt outptr
  , BinToInt continuation
  , BinSub p outptr continuation
  , Change (SingleEdge p) b igraph
  , Change (SingleEdge continuation) a igraph
  ) =>
  Change (SingleEdge p) (Dup a (Proxy skolem -> b)) igraph where
  change' _ (Dup a f) = FrameT (_1 *> _2)
    where
    FrameT _1 = (change' :: ChangeType (SingleEdge p) b igraph) Proxy (f Proxy)

    FrameT _2 = (change' :: ChangeType (SingleEdge continuation) a igraph) Proxy a

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
  , Change nextP argC igraph
  ) =>
  Change (SingleEdge p) (CTOR.Allpass argA argB fOfargC) igraph where
  change' _ (CTOR.Allpass argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Allpass argA argB argC)
        (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeBandpass ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Bandpass argA argB argC) p igraph nextP
  , Change nextP argC igraph
  ) =>
  Change (SingleEdge p) (CTOR.Bandpass argA argB fOfargC) igraph where
  change' _ (CTOR.Bandpass argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Bandpass argA argB argC)
        (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeConstant ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.Constant argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.Constant argA) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeDelay ::
  ( SetterVal argA
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , Modify (CTOR.Delay argA argB) p igraph nextP
  , Change nextP argB igraph
  ) =>
  Change (SingleEdge p) (CTOR.Delay argA fOfargB) igraph where
  change' _ (CTOR.Delay argA fOfargB) =
    let
      argB = (((toSkolemizedFunction :: fOfargB -> (Proxy skolem -> argB)) fOfargB) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Delay argA argB)
        (change' :: ChangeType nextP argB igraph) Proxy argB

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
  , Change nextP argF igraph
  ) =>
  Change (SingleEdge p) (CTOR.DynamicsCompressor argA argB argC argD argE fOfargF) igraph where
  change' _ (CTOR.DynamicsCompressor argA argB argC argD argE fOfargF) =
    let
      argF = (((toSkolemizedFunction :: fOfargF -> (Proxy skolem -> argF)) fOfargF) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.DynamicsCompressor argA argB argC argD argE argF)
        (change' :: ChangeType nextP argF igraph) Proxy argF

instance changeHighpass ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Highpass argA argB argC) p igraph nextP
  , Change nextP argC igraph
  ) =>
  Change (SingleEdge p) (CTOR.Highpass argA argB fOfargC) igraph where
  change' _ (CTOR.Highpass argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Highpass argA argB argC)
        (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeHighshelf ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Highshelf argA argB argC) p igraph nextP
  , Change nextP argC igraph
  ) =>
  Change (SingleEdge p) (CTOR.Highshelf argA argB fOfargC) igraph where
  change' _ (CTOR.Highshelf argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Highshelf argA argB argC)
        (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeLoopBuf ::
  ( SetterVal argB
  , BinToInt p
  , Modify (CTOR.LoopBuf argA argB) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.LoopBuf argA argB) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeLowpass ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Lowpass argA argB argC) p igraph nextP
  , Change nextP argC igraph
  ) =>
  Change (SingleEdge p) (CTOR.Lowpass argA argB fOfargC) igraph where
  change' _ (CTOR.Lowpass argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Lowpass argA argB argC)
        (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeLowshelf ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Lowshelf argA argB argC) p igraph nextP
  , Change nextP argC igraph
  ) =>
  Change (SingleEdge p) (CTOR.Lowshelf argA argB fOfargC) igraph where
  change' _ (CTOR.Lowshelf argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Lowshelf argA argB argC)
        (change' :: ChangeType nextP argC igraph) Proxy argC

instance changeNotch ::
  ( SetterVal argA
  , SetterVal argB
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , Modify (CTOR.Notch argA argB argC) p igraph nextP
  , Change nextP argC igraph
  ) =>
  Change (SingleEdge p) (CTOR.Notch argA argB fOfargC) igraph where
  change' _ (CTOR.Notch argA argB fOfargC) =
    let
      argC = (((toSkolemizedFunction :: fOfargC -> (Proxy skolem -> argC)) fOfargC) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Notch argA argB argC)
        (change' :: ChangeType nextP argC igraph) Proxy argC

instance changePeaking ::
  ( SetterVal argA
  , SetterVal argB
  , SetterVal argC
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargD skolem
  , ToSkolemizedFunction fOfargD skolem argD
  , Modify (CTOR.Peaking argA argB argC argD) p igraph nextP
  , Change nextP argD igraph
  ) =>
  Change (SingleEdge p) (CTOR.Peaking argA argB argC fOfargD) igraph where
  change' _ (CTOR.Peaking argA argB argC fOfargD) =
    let
      argD = (((toSkolemizedFunction :: fOfargD -> (Proxy skolem -> argD)) fOfargD) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.Peaking argA argB argC argD)
        (change' :: ChangeType nextP argD igraph) Proxy argD

instance changePeriodicOsc ::
  ( SetterVal argB
  , BinToInt p
  , Modify (CTOR.PeriodicOsc argA argB) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.PeriodicOsc argA argB) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changePlayBuf ::
  ( SetterVal argB
  , BinToInt p
  , Modify (CTOR.PlayBuf argA argB) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.PlayBuf argA argB) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeSawtoothOsc ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.SawtoothOsc argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.SawtoothOsc argA) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeSinOsc ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.SinOsc argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.SinOsc argA) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeSquareOsc ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.SquareOsc argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.SquareOsc argA) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeStereoPanner ::
  ( SetterVal argA
  , BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , Modify (CTOR.StereoPanner argA argB) p igraph nextP
  , Change nextP argB igraph
  ) =>
  Change (SingleEdge p) (CTOR.StereoPanner argA fOfargB) igraph where
  change' _ (CTOR.StereoPanner argA fOfargB) =
    let
      argB = (((toSkolemizedFunction :: fOfargB -> (Proxy skolem -> argB)) fOfargB) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (CTOR.StereoPanner argA argB)
        (change' :: ChangeType nextP argB igraph) Proxy argB

instance changeTriangleOsc ::
  ( SetterVal argA
  , BinToInt p
  , Modify (CTOR.TriangleOsc argA) p igraph nextP
  ) =>
  Change (SingleEdge p) (CTOR.TriangleOsc argA) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

--------------
instance changeGain ::
  ( SetterVal a
  , BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , Modify (Gain a b) p igraph nextP
  , Change nextP b igraph
  ) =>
  Change (SingleEdge p) (Gain a fb) igraph where
  change' _ (Gain a fb) =
    let
      b = (((toSkolemizedFunction :: fb -> (Proxy skolem -> b)) fb) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (Gain a b)
        (change' :: ChangeType nextP b igraph) Proxy b

instance changeSpeaker ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fa skolem
  , ToSkolemizedFunction fa skolem a
  , Modify (Speaker a) p igraph nextP
  , Change nextP a igraph
  ) =>
  Change (SingleEdge p) (Speaker fa) igraph where
  change' _ (Speaker fa) =
    let
      a = (((toSkolemizedFunction :: fa -> (Proxy skolem -> a)) fa) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (Speaker a)
        (change' :: ChangeType nextP a igraph) Proxy a
