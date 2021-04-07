module WAGS.Create where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Set as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Frame(..), AudioState)
import WAGS.Graph.Constructors (Dup(..), Gain, Speaker)
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Decorators (Focus(..))
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam)
import WAGS.Rendered (AnAudioUnit(..), Instruction(..))
import WAGS.Universe.AudioUnit (AudioUnitRef(..), TGain, TSpeaker)
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinSucc, class BinToInt, BinL, PtrList, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, GraphC)
import WAGS.Universe.Node (NodeC)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class GetSkolemizedFunctionFromAU, class LookupSkolem, class MakeInternalSkolemStack, class SkolemNotYetPresent, class SkolemNotYetPresentOrDiscardable, class ToSkolemizedFunction, DiscardableSkolem, SkolemListCons, SkolemPairC, getSkolemizedFunctionFromAU)
import WAGS.Universe.Universe (Universe, UniverseC)

newtype PtrArr :: forall k. k -> Type
newtype PtrArr a
  = PtrArr (Array Int)

class EdgeListable a (b :: PtrList) | a -> b where
  getPointers' :: a -> PtrArr b

instance edgeListableUnit :: EdgeListable Unit PtrListNil where
  getPointers' _ = PtrArr []

instance edgeListableTuple :: EdgeListable x y => EdgeListable (Tuple (AudioUnitRef ptr) x) (PtrListCons ptr y) where
  getPointers' (Tuple (AudioUnitRef i) x) = let PtrArr o = getPointers' x in PtrArr ([ i ] <> o)

class AsEdgeProfile a (b :: EdgeProfile) | a -> b where
  getPointers :: a -> PtrArr b

instance asEdgeProfileAR :: AsEdgeProfile (AudioUnitRef ptr) (SingleEdge ptr) where
  getPointers (AudioUnitRef i) = PtrArr [ i ]

instance asEdgeProfileTupl :: EdgeListable x y => AsEdgeProfile (Tuple (AudioUnitRef ptr) x) (ManyEdges ptr y) where
  getPointers (Tuple (AudioUnitRef i) el) = let PtrArr o = getPointers' el in PtrArr ([ i ] <> o)

class InitialVal a where
  initialVal :: a -> AudioParameter

instance initialValNumber :: InitialVal Number where
  initialVal a = AudioParameter $ defaultParam { param = a }

instance initialValAudioParameter :: InitialVal AudioParameter where
  initialVal = identity

instance initialValTuple :: InitialVal a => InitialVal (Tuple a b) where
  initialVal = initialVal <<< fst

class CreationInstructions (g :: Type) where
  creationInstructions :: Int -> g -> Array Instruction /\ AnAudioUnit

{-
instance creationInstructionsSinOsc :: InitialVal a => CreationInstructions (SinOsc a) where
  creationInstructions idx (SinOsc a) =
    let
      iv' = initialVal a

      AudioParameter iv = iv'
    in
      [ NewUnit idx "sinosc"
      , SetFrequency idx iv.param iv.timeOffset iv.transition
      ]
        /\ ASinOsc iv'

instance creationInstructionsHighpass :: (InitialVal a, InitialVal b) => CreationInstructions (Highpass a b c) where
  creationInstructions idx (Highpass a b _) =
    let
      aiv' = initialVal a

      biv' = initialVal b

      AudioParameter aiv = aiv'

      AudioParameter biv = biv'
    in
      [ NewUnit idx "highpass"
      , SetFrequency idx aiv.param aiv.timeOffset aiv.transition
      , SetQ idx biv.param biv.timeOffset biv.transition
      ]
        /\ AHighpass aiv' biv'

instance creationInstructionsGain :: InitialVal a => CreationInstructions (Gain a b) where
  creationInstructions idx (Gain a _) =
    let
      iv' = initialVal a

      AudioParameter iv = iv'
    in
      [ NewUnit idx "gain"
      , SetGain idx iv.param iv.timeOffset iv.transition
      ]
        /\ AGain iv'

instance creationInstructionsSpeaker :: CreationInstructions (Speaker a) where
  creationInstructions idx (Speaker _) = [] /\ ASpeaker


-}
instance creationInstructionsAllpass :: (InitialVal argA, InitialVal argB) => CreationInstructions (CTOR.Allpass argA argB argC) where
  creationInstructions idx (CTOR.Allpass argA argB _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetQ idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetGain idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
    in
      ([ NewUnit idx "allpass" ] <> argA_StartsWith <> argB_StartsWith)
        /\ AAllpass argA_iv' argB_iv'

instance creationInstructionsBandpass :: (InitialVal argA, InitialVal argB) => CreationInstructions (CTOR.Bandpass argA argB argC) where
  creationInstructions idx (CTOR.Bandpass argA argB _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
    in
      ([ NewUnit idx "bandpass" ] <> argA_StartsWith <> argB_StartsWith)
        /\ ABandpass argA_iv' argB_iv'

instance creationInstructionsConstant :: (InitialVal argA) => CreationInstructions (CTOR.Constant argA) where
  creationInstructions idx (CTOR.Constant argA) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetOffset idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
    in
      ([ NewUnit idx "constant" ] <> argA_StartsWith)
        /\ AConstant argA_iv'

instance creationInstructionsConvolver :: CreationInstructions (CTOR.Convolver argA argB) where
  creationInstructions _ _ = [] /\ ASpeaker

instance creationInstructionsDelay :: (InitialVal argA) => CreationInstructions (CTOR.Delay argA argB) where
  creationInstructions idx (CTOR.Delay argA _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetDelay idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
    in
      ([ NewUnit idx "delay" ] <> argA_StartsWith)
        /\ ADelay argA_iv'

instance creationInstructionsDynamicsCompressor :: (InitialVal argA, InitialVal argB, InitialVal argC, InitialVal argD, InitialVal argE) => CreationInstructions (CTOR.DynamicsCompressor argA argB argC argD argE argF) where
  creationInstructions idx (CTOR.DynamicsCompressor argA argB argC argD argE _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetThreshold idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetKnee idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]

      argC_iv' = initialVal argC

      argC_StartsWith = let AudioParameter argC_iv = argC_iv' in [ SetRatio idx argC_iv.param argC_iv.timeOffset argC_iv.transition ]

      argD_iv' = initialVal argD

      argD_StartsWith = let AudioParameter argD_iv = argD_iv' in [ SetAttack idx argD_iv.param argD_iv.timeOffset argD_iv.transition ]

      argE_iv' = initialVal argE

      argE_StartsWith = let AudioParameter argE_iv = argE_iv' in [ SetRelease idx argE_iv.param argE_iv.timeOffset argE_iv.transition ]
    in
      ([ NewUnit idx "dynamicscompressor" ] <> argA_StartsWith <> argB_StartsWith <> argC_StartsWith <> argD_StartsWith <> argE_StartsWith)
        /\ ADynamicsCompressor argA_iv' argB_iv' argC_iv' argD_iv' argE_iv'

instance creationInstructionsGain :: (InitialVal argA) => CreationInstructions (CTOR.Gain argA argB) where
  creationInstructions idx (CTOR.Gain argA _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetGain idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
    in
      ([ NewUnit idx "gain" ] <> argA_StartsWith)
        /\ AGain argA_iv'

instance creationInstructionsHighpass :: (InitialVal argA, InitialVal argB) => CreationInstructions (CTOR.Highpass argA argB argC) where
  creationInstructions idx (CTOR.Highpass argA argB _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
    in
      ([ NewUnit idx "highpass" ] <> argA_StartsWith <> argB_StartsWith)
        /\ AHighpass argA_iv' argB_iv'

instance creationInstructionsHighshelf :: (InitialVal argA, InitialVal argB) => CreationInstructions (CTOR.Highshelf argA argB argC) where
  creationInstructions idx (CTOR.Highshelf argA argB _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetGain idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
    in
      ([ NewUnit idx "highshelf" ] <> argA_StartsWith <> argB_StartsWith)
        /\ AHighshelf argA_iv' argB_iv'

instance creationInstructionsLoopBuf :: (IsSymbol argA, InitialVal argB) => CreationInstructions (CTOR.LoopBuf argA argB) where
  creationInstructions idx (CTOR.LoopBuf px argB loopStart loopEnd) =
    let
      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetPlaybackRate idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      bufname = reflectSymbol px
    in
      ([ NewUnit idx "loopbuf" ] <> argB_StartsWith)
        /\ ALoopBuf bufname argB_iv' loopStart loopEnd

instance creationInstructionsLowpass :: (InitialVal argA, InitialVal argB) => CreationInstructions (CTOR.Lowpass argA argB argC) where
  creationInstructions idx (CTOR.Lowpass argA argB _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
    in
      ([ NewUnit idx "lowpass" ] <> argA_StartsWith <> argB_StartsWith)
        /\ ALowpass argA_iv' argB_iv'

instance creationInstructionsLowshelf :: (InitialVal argA, InitialVal argB) => CreationInstructions (CTOR.Lowshelf argA argB argC) where
  creationInstructions idx (CTOR.Lowshelf argA argB _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetGain idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
    in
      ([ NewUnit idx "lowshelf" ] <> argA_StartsWith <> argB_StartsWith)
        /\ ALowshelf argA_iv' argB_iv'

instance creationInstructionsMicrophone :: CreationInstructions (CTOR.Microphone) where
  creationInstructions _ _ = [] /\ ASpeaker

instance creationInstructionsNotch :: (InitialVal argA, InitialVal argB) => CreationInstructions (CTOR.Notch argA argB argC) where
  creationInstructions idx (CTOR.Notch argA argB _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
    in
      ([ NewUnit idx "notch" ] <> argA_StartsWith <> argB_StartsWith)
        /\ ANotch argA_iv' argB_iv'

instance creationInstructionsPeaking :: (InitialVal argA, InitialVal argB, InitialVal argC) => CreationInstructions (CTOR.Peaking argA argB argC argD) where
  creationInstructions idx (CTOR.Peaking argA argB argC _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]

      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetQ idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]

      argC_iv' = initialVal argC

      argC_StartsWith = let AudioParameter argC_iv = argC_iv' in [ SetGain idx argC_iv.param argC_iv.timeOffset argC_iv.transition ]
    in
      ([ NewUnit idx "peaking" ] <> argA_StartsWith <> argB_StartsWith <> argC_StartsWith)
        /\ APeaking argA_iv' argB_iv' argC_iv'

instance creationInstructionsPeriodicOsc :: (IsSymbol argA, InitialVal argB) => CreationInstructions (CTOR.PeriodicOsc argA argB) where
  creationInstructions idx (CTOR.PeriodicOsc px argB) =
    let
      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetFrequency idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      oscname = reflectSymbol px
    in
      ([ NewPeriodicOsc idx oscname ] <> argB_StartsWith)
        /\ APeriodicOsc oscname argB_iv'

instance creationInstructionsPlayBuf :: (IsSymbol argA, InitialVal argB) => CreationInstructions (CTOR.PlayBuf argA argB) where
  creationInstructions idx (CTOR.PlayBuf px offset argB) =
    let
      argB_iv' = initialVal argB

      argB_StartsWith = let AudioParameter argB_iv = argB_iv' in [ SetPlaybackRate idx argB_iv.param argB_iv.timeOffset argB_iv.transition ]
      bufname = reflectSymbol px
    in
      ([ NewPlayBuf idx bufname offset ] <> argB_StartsWith)
        /\ APlayBuf bufname offset argB_iv'

instance creationInstructionsRecorder :: CreationInstructions (CTOR.Recorder argA argB) where
  creationInstructions _ _ = [] /\ ASpeaker

instance creationInstructionsSawtoothOsc :: (InitialVal argA) => CreationInstructions (CTOR.SawtoothOsc argA) where
  creationInstructions idx (CTOR.SawtoothOsc argA) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
    in
      ([ NewUnit idx "sawtoothosc" ] <> argA_StartsWith)
        /\ ASawtoothOsc argA_iv'

instance creationInstructionsSinOsc :: (InitialVal argA) => CreationInstructions (CTOR.SinOsc argA) where
  creationInstructions idx (CTOR.SinOsc argA) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
    in
      ([ NewUnit idx "sinosc" ] <> argA_StartsWith)
        /\ ASinOsc argA_iv'

instance creationInstructionsSpeaker :: CreationInstructions (CTOR.Speaker argA) where
  creationInstructions _ _ = [] /\ ASpeaker

instance creationInstructionsSquareOsc :: (InitialVal argA) => CreationInstructions (CTOR.SquareOsc argA) where
  creationInstructions idx (CTOR.SquareOsc argA) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
    in
      ([ NewUnit idx "squareosc" ] <> argA_StartsWith)
        /\ ASquareOsc argA_iv'

instance creationInstructionsStereoPanner :: (InitialVal argA) => CreationInstructions (CTOR.StereoPanner argA argB) where
  creationInstructions idx (CTOR.StereoPanner argA _) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetPan idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
    in
      ([ NewUnit idx "stereopanner" ] <> argA_StartsWith)
        /\ AStereoPanner argA_iv'

instance creationInstructionsTriangleOsc :: (InitialVal argA) => CreationInstructions (CTOR.TriangleOsc argA) where
  creationInstructions idx (CTOR.TriangleOsc argA) =
    let
      argA_iv' = initialVal argA

      argA_StartsWith = let AudioParameter argA_iv = argA_iv' in [ SetFrequency idx argA_iv.param argA_iv.timeOffset argA_iv.transition ]
    in
      ([ NewUnit idx "triangleosc" ] <> argA_StartsWith)
        /\ ATriangleOsc argA_iv'

instance creationInstructionsWaveShaper :: CreationInstructions (CTOR.WaveShaper argA argB argC) where
  creationInstructions _ _ = [] /\ ASpeaker

class Create (a :: Type) (i :: Universe) (o :: Universe) (x :: Type) | a i -> o x where
  create :: forall env proof. a -> Frame env proof i o x

creationStep ::
  forall env proof g.
  CreationInstructions g =>
  g ->
  AudioState proof env Int
creationStep g = do
  currentIdx <- gets _.currentIdx
  let
    renderable /\ internal = creationInstructions currentIdx g
  modify_
    ( \i ->
        i
          { currentIdx = (currentIdx + 1)
          , internalNodes = (M.insert currentIdx (internal) i.internalNodes)
          , instructions = i.instructions <> renderable
          }
    )
  pure currentIdx

type ProxyCC skolem ptr innerTerm i o
  = Proxy (skolem /\ ptr /\ innerTerm /\ i /\ o)

createAndConnect ::
  forall env proof g (ptr :: BinL) skolem c (i :: Universe) (o :: Universe) innerTerm eprof.
  GetSkolemizedFunctionFromAU g skolem c =>
  AsEdgeProfile innerTerm eprof =>
  CreationInstructions g =>
  Create c i o innerTerm =>
  Proxy (skolem /\ (Proxy ptr) /\ innerTerm /\ (Proxy i) /\ (Proxy o)) ->
  g ->
  Frame env proof i o Int
createAndConnect _ g =
  Frame
    $ do
        idx <- cs
        let
          (Frame (mc)) =
            (create :: c -> Frame env proof i o innerTerm)
              ( ((getSkolemizedFunctionFromAU :: g -> (Proxy skolem -> c)) g)
                  Proxy
              )
        oc <- mc
        let
          PtrArr o = getPointers oc
        modify_
          ( \i ->
              i
                { internalEdges =
                  (M.insertWith S.union idx (S.fromFoldable o) i.internalEdges)
                , instructions =
                  i.instructions
                    <> map (flip ConnectXToY idx) o
                }
          )
        pure idx
  where
  cs = creationStep g

-- end of the line in tuples
instance createUnit ::
  Create Unit u u Unit where
  create = Frame <<< pure

instance createTuple ::
  (Create x u0 u1 x', Create y u1 u2 y') =>
  Create (x /\ y) u0 u2 (x' /\ y') where
  create (x /\ y) = (Frame) $ Tuple <$> x' <*> y'
    where
    Frame (x') = (create :: forall env proof. x -> Frame env proof u0 u1 x') x

    Frame (y') = (create :: forall env proof. y -> Frame env proof u1 u2 y') y

instance createIdentity :: Create x i o r => Create (Identity x) i o r where
  create (Identity x) = create x

instance createFocus :: Create x i o r => Create (Focus x) i o r where
  create (Focus x) = create x

instance createProxy ::
  ( LookupSkolem skolem skolems ptr
  , BinToInt ptr
  ) =>
  Create
    (Proxy skolem)
    (UniverseC next graph changeBit skolems)
    (UniverseC next graph changeBit skolems)
    (AudioUnitRef ptr) where
  create _ = Frame $ (pure $ AudioUnitRef $ toInt' (Proxy :: Proxy ptr))

instance createDup ::
  ( SkolemNotYetPresent skolem skolems
  , BinToInt ptr
  , Create
      a
      (UniverseC ptr graphi changeBit skolems)
      (UniverseC midptr graphm changeBit skolems)
      ignore
  , Create
      b
      (UniverseC midptr graphm changeBit (SkolemListCons (SkolemPairC skolem ptr) skolems))
      (UniverseC outptr grapho changeBit (SkolemListCons (SkolemPairC skolem ptr) skolems))
      (AudioUnitRef midptr)
  ) =>
  Create
    (Dup a (Proxy skolem -> b))
    (UniverseC ptr graphi changeBit skolems)
    (UniverseC outptr grapho changeBit skolems)
    (AudioUnitRef midptr) where
  create (Dup a f) = Frame $ x *> y
    where
    Frame x =
      ( create ::
          forall env proof.
          a ->
          Frame env proof
            (UniverseC ptr graphi changeBit skolems)
            (UniverseC midptr graphm changeBit skolems)
            ignore
      )
        a

    Frame y =
      ( create ::
          forall env proof.
          b ->
          Frame env proof
            (UniverseC midptr graphm changeBit (SkolemListCons (SkolemPairC skolem ptr) skolems))
            (UniverseC outptr grapho changeBit (SkolemListCons (SkolemPairC skolem ptr) skolems))
            (AudioUnitRef midptr)
      )
        (f (Proxy :: _ skolem))
{-
instance createSinOsc ::
  ( InitialVal a
  , BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (SinOsc a)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep

instance createHighpass ::
  ( InitialVal a
  , InitialVal b
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      c
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Highpass a b fc)
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (THighpass ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
-}
instance createAllpass::
  (InitialVal argA, InitialVal argB,
  
    GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argC
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Allpass argA argB fOfargC) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TAllpass ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createBandpass::
  (InitialVal argA, InitialVal argB,
  
    GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argC
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Bandpass argA argB fOfargC) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TBandpass ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createConstant::
  ( InitialVal argA, BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.Constant argA)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TConstant ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  
instance createConvolver::
  (
  
    GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argB
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Convolver argA fOfargB) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TConvolver ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createDelay::
  (InitialVal argA,
  
    GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argB
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Delay argA fOfargB) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TDelay ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createDynamicsCompressor::
  (InitialVal argA, InitialVal argB, InitialVal argC, InitialVal argD, InitialVal argE,
  
    GetSkolemFromRecursiveArgument fOfargF skolem
  , ToSkolemizedFunction fOfargF skolem argF
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argF
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.DynamicsCompressor argA argB argC argD argE fOfargF) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TDynamicsCompressor ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createHighpass::
  (InitialVal argA, InitialVal argB,
  
    GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argC
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Highpass argA argB fOfargC) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.THighpass ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createHighshelf::
  (InitialVal argA, InitialVal argB,
  
    GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argC
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Highshelf argA argB fOfargC) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.THighshelf ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createLoopBuf::
  ( InitialVal argB, BinToInt ptr
  , BinSucc ptr next
  , IsSymbol argA
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.LoopBuf argA argB)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TLoopBuf ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  
instance createLowpass::
  (InitialVal argA, InitialVal argB,
  
    GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argC
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Lowpass argA argB fOfargC) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TLowpass ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createLowshelf::
  (InitialVal argA, InitialVal argB,
  
    GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argC
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Lowshelf argA argB fOfargC) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TLowshelf ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createMicrophone::
  (  BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.Microphone)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TMicrophone ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  
instance createNotch::
  (InitialVal argA, InitialVal argB,
  
    GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argC
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Notch argA argB fOfargC) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TNotch ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createPeaking::
  (InitialVal argA, InitialVal argB, InitialVal argC,
  
    GetSkolemFromRecursiveArgument fOfargD skolem
  , ToSkolemizedFunction fOfargD skolem argD
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argD
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Peaking argA argB argC fOfargD) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TPeaking ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createPeriodicOsc::
  ( InitialVal argB, BinToInt ptr
  , BinSucc ptr next
  , IsSymbol argA
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.PeriodicOsc argA argB)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TPeriodicOsc ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  
instance createPlayBuf::
  ( InitialVal argB, BinToInt ptr
  , BinSucc ptr next
  , IsSymbol argA
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.PlayBuf argA argB)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TPlayBuf ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  
instance createRecorder::
  (
  
    GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argB
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.Recorder argA fOfargB) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TRecorder ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createSawtoothOsc::
  ( InitialVal argA, BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.SawtoothOsc argA)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TSawtoothOsc ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  
instance createSinOsc::
  ( InitialVal argA, BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.SinOsc argA)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TSinOsc ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  

instance createSquareOsc::
  ( InitialVal argA, BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.SquareOsc argA)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TSquareOsc ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  
instance createStereoPanner::
  (InitialVal argA,
  
    GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argB
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.StereoPanner argA fOfargB) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TStereoPanner ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))
instance createTriangleOsc::
  ( InitialVal argA, BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (CTOR.TriangleOsc argA)
    (UniverseC ptr graph changeBit skolems)
    ( UniverseC next
        (GraphC (NodeC (AU.TTriangleOsc ptr) NoEdge) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create = Frame <<< (map) AudioUnitRef <<< creationStep
  
instance createWaveShaper::
  (
  
    GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      argC
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (CTOR.WaveShaper argA argB fOfargC) 
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (AU.TWaveShaper ptr) (SingleEdge op)) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

-----------------------
------------------------
instance createGain ::
  ( InitialVal a
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , SkolemNotYetPresentOrDiscardable skolem skolems
  , MakeInternalSkolemStack skolem ptr skolems skolemsInternal
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      b
      (UniverseC next graphi changeBit skolemsInternal)
      (UniverseC outptr grapho changeBit skolemsInternal)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Gain a fb)
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (TGain ptr) eprof) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< (createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal))))

instance createSpeaker ::
  ( ToSkolemizedFunction a DiscardableSkolem a
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      a
      (UniverseC next graphi changeBit skolems)
      (UniverseC outptr grapho changeBit skolems)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Speaker a)
    (UniverseC ptr graphi changeBit skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (TSpeaker ptr) eprof) nodeList)
        changeBit
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< (createAndConnect (Proxy :: ProxyCC DiscardableSkolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolems)) (Proxy (UniverseC outptr grapho changeBit skolems))))

----------
