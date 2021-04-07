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
import WAGS.Control.Types (Frame(..))
import WAGS.Create (class Create)
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Constructors (Dup(..), Gain(..), Highpass(..), SinOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..))
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam, param)
import WAGS.Rendered (AnAudioUnit(..), Instruction(..))
import WAGS.Universe.AudioUnit (AudioUnitRef, TGain, THighpass, TSinOsc, TSpeaker)
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
    ALoopBuf v_argB@(AudioParameter v_argB') ->
      let
        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetPlaybackRate idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argB_Changes)
          /\ ALoopBuf argB_iv'
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

instance changeInstructionsMicrophone :: ChangeInstructions (CTOR.Microphone argA) where
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
    APeriodicOsc v_argB@(AudioParameter v_argB') ->
      let
        s_argB = setterVal argB

        argB_iv' = s_argB v_argB

        argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param then [] else [ SetFrequency idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      in
        Just
          $ (argB_Changes)
          /\ APeriodicOsc argB_iv'
    _ -> Nothing

instance changeInstructionsPlayBuf :: (SetterVal argC) => ChangeInstructions (CTOR.PlayBuf argA argC) where
  changeInstructions idx (CTOR.PlayBuf _ _ argC) = case _ of
    APlayBuf v_argC@(AudioParameter v_argC') ->
      let
        s_argC = setterVal argC

        argC_iv' = s_argC v_argC

        argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param then [] else [ SetPlaybackRate idx argC_iv.param argC_iv.timeOffset argC_iv.transition ]
      in
        Just
          $ (argC_Changes)
          /\ APlayBuf argC_iv'
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
{-
instance changeInstructionsSinOsc :: SetterVal a => ChangeInstructions (SinOsc a) where
  changeInstructions idx (SinOsc a) = case _ of
    ASinOsc prm@(AudioParameter prm') ->
      Just $ setterVal a
        # \f ->
            let
              iv' = f prm

              AudioParameter iv = iv'
            in
              (if iv.param == prm'.param then [] else [ SetFrequency idx iv.param iv.timeOffset iv.transition ]) /\ ASinOsc iv'
    _ -> Nothing

instance changeInstructionsHighpass :: (SetterVal a, SetterVal b) => ChangeInstructions (Highpass a b c) where
  changeInstructions idx (Highpass a b _) = case _ of
    AHighpass va@(AudioParameter va') vb@(AudioParameter vb') ->
      let
        sa = setterVal a

        aiv' = sa va

        freqChanges = let AudioParameter aiv = aiv' in if aiv.param == va'.param then [] else [ SetFrequency idx aiv.param aiv.timeOffset aiv.transition ]

        sb = setterVal b

        biv' = sb vb

        qChanges = let AudioParameter biv = biv' in if biv.param == vb'.param then [] else [ SetQ idx biv.param biv.timeOffset biv.transition ]
      in
        Just
          $ (freqChanges <> qChanges)
          /\ AHighpass aiv' biv'
    _ -> Nothing

instance changeInstructionsGain :: SetterVal a => ChangeInstructions (Gain a b) where
  changeInstructions idx (Gain a _) fromMap = case fromMap of
    AGain prm@(AudioParameter prm') ->
      Just $ setterVal a
        # \f ->
            let
              iv' = f prm

              AudioParameter iv = iv'
            in
              (if iv.param == prm'.param then [] else [ SetGain idx iv.param iv.timeOffset iv.transition ]) /\ AGain iv'
    _ -> Nothing

instance changeInstructionsSpeaker :: ChangeInstructions (Speaker a) where
  changeInstructions _ _ _ = Nothing
-}
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
  forall edge a currentIdx graph changeBit skolems env proof.
  TerminalIdentityEdge graph edge =>
  Change edge a graph =>
  a -> Frame env proof (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx graph (Succ changeBit) skolems) Unit
change = change' (Proxy :: _ edge)

changeAt ::
  forall ptr a env proof currentIdx graph changeBit skolems.
  Change (SingleEdge ptr) a graph =>
  AudioUnitRef ptr -> a -> Frame env proof (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx graph (Succ changeBit) skolems) Unit
changeAt _ = change' (Proxy :: _ (SingleEdge ptr))

type ChangeType (p :: EdgeProfile) (a :: Type) (grapho :: Graph)
  = forall env proof ptr changeBit skolems. Proxy p -> a -> Frame env proof (UniverseC ptr grapho changeBit skolems) (UniverseC ptr grapho (Succ changeBit) skolems) Unit

type ChangesType (a :: Type) (g :: Graph)
  = forall env proof ptr changeBit skolems. a -> Frame env proof (UniverseC ptr g changeBit skolems) (UniverseC ptr g (Succ changeBit) skolems) Unit

class Changes (a :: Type) (g :: Graph) where
  changes :: forall env proof ptr changeBit skolems. a -> Frame env proof (UniverseC ptr g changeBit skolems) (UniverseC ptr g (Succ changeBit) skolems) Unit

data ChangeInstruction a b
  = ChangeInstruction a b

instance changesUnit :: Changes Unit g where
  changes _ = Frame (pure unit)
else instance changesPx :: Change p a graph => Changes (ChangeInstruction (Proxy p) a) graph where
  changes (ChangeInstruction p a) = (change' :: ChangeType p a graph) p a
else instance changesTp :: (Changes x graph, Changes y graph) => Changes (Tuple x y) graph where
  changes (x /\ y) = Frame (x' *> y')
    where
    Frame x' = (changes :: ChangesType x graph) x

    Frame y' = (changes :: ChangesType y graph) y
else instance changesSingle :: (TerminalIdentityEdge graph edge, Change edge a graph) => Changes a graph where
  changes a = change a

class Change (p :: EdgeProfile) (a :: Type) (grapho :: Graph) where
  change' :: forall env proof ptr changeBit skolems. Proxy p -> a -> Frame env proof (UniverseC ptr grapho changeBit skolems) (UniverseC ptr grapho (Succ changeBit) skolems) Unit

class ModifyRes (tag :: Type) (p :: Ptr) (i :: Node) (mod :: NodeList) (plist :: EdgeProfile) | tag p i -> mod plist

instance modifyResSinOsc :: ModifyRes (SinOsc a) p (NodeC (TSinOsc p) e) (NodeListCons (NodeC (TSinOsc p) e) NodeListNil) e
else instance modifyResHighpass :: ModifyRes (Highpass a b c) p (NodeC (THighpass p) e) (NodeListCons (NodeC (THighpass p) e) NodeListNil) e
else instance modifyResGain :: ModifyRes (Gain a b) p (NodeC (TGain p) e) (NodeListCons (NodeC (TGain p) e) NodeListNil) e
else instance modifyResSpeaker :: ModifyRes (Speaker a) p (NodeC (TSpeaker p) e) (NodeListCons (NodeC (TSpeaker p) e) NodeListNil) e
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
  forall g env proof currentIdx (igraph :: Graph) changeBit skolems (p :: BinL) (nextP :: EdgeProfile) univ.
  ChangeInstructions g =>
  BinToInt p =>
  Modify g p igraph nextP =>
  Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph) -> g -> Frame env proof univ (UniverseC currentIdx igraph changeBit skolems) Unit
changeAudioUnit _ g =
  Frame
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
  change' _ _ = Frame $ (pure unit)

instance changeSkolem ::
  Change (SingleEdge p) (Proxy skolem) igraph where
  change' _ _ = Frame $ (pure unit)

instance changeIdentity :: Change (SingleEdge p) x igraph => Change (SingleEdge p) (Identity x) igraph where
  change' p (Identity x) = change' p x

instance changeFocus :: Change (SingleEdge p) x igraph => Change (SingleEdge p) (Focus x) igraph where
  change' p (Focus x) = change' p x

instance changeMany2 ::
  ( Change (SingleEdge p) x igraph
  , Change (ManyEdges a b) y igraph
  ) =>
  Change (ManyEdges p (PtrListCons a b)) (x /\ y) igraph where
  change' _ (x /\ y) = Frame (_1 *> _2)
    where
    Frame _1 = (change' :: ChangeType (SingleEdge p) x igraph) Proxy x

    Frame _2 = (change' :: ChangeType (ManyEdges a b) y igraph) Proxy y

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
  change' _ (Dup a f) = Frame (_1 *> _2)
    where
    Frame _1 = (change' :: ChangeType (SingleEdge p) b igraph) Proxy (f Proxy)

    Frame _2 = (change' :: ChangeType (SingleEdge continuation) a igraph) Proxy a

instance changeSinOsc ::
  ( SetterVal a
  , BinToInt p
  , Modify (SinOsc a) p igraph nextP
  ) =>
  Change (SingleEdge p) (SinOsc a) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeHighpass ::
  ( SetterVal a
  , SetterVal b
  , BinToInt p
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , Modify (Highpass a b c) p igraph nextP
  , Change nextP c igraph
  ) =>
  Change (SingleEdge p) (Highpass a b fc) igraph where
  change' _ (Highpass a b fc) =
    let
      c = (((toSkolemizedFunction :: fc -> (Proxy skolem -> c)) fc) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (Highpass a b c)
        (change' :: ChangeType nextP c igraph) Proxy c

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
