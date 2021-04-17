module WAGS.Create
  ( class Create
  , class CreationInstructions
  , class InitialVal
  , ProxyCC
  , create
  , creationInstructions
  , initialVal
  ) where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Set as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (AudioState, FrameT, unsafeFrame, unsafeUnframe)
import WAGS.Graph.Constructors (Dup(..), Gain, Speaker)
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Decorators (Focus(..), class IsOversample, reflectOversample)
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam)
import WAGS.Interpret (class AudioInterpret, connectXToY, makeAllpass, makeBandpass, makeConstant, makeConvolver, makeDelay, makeDynamicsCompressor, makeGain, makeHighpass, makeHighshelf, makeLoopBuf, makeLowpass, makeLowshelf, makeMicrophone, makeNotch, makePeaking, makePeriodicOsc, makePlayBuf, makeRecorder, makeSawtoothOsc, makeSinOsc, makeSpeaker, makeSquareOsc, makeStereoPanner, makeTriangleOsc, makeWaveShaper)
import WAGS.Rendered (AnAudioUnit(..))
import WAGS.Universe.AudioUnit (AudioUnitRef(..), TGain, TSpeaker)
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinSucc, class BinToInt, BinL, toInt')
import WAGS.Universe.EdgeProfile (class AsEdgeProfile, NoEdge, PtrArr(..), SingleEdge, getPointers)
import WAGS.Universe.Graph (class GraphToNodeList, GraphC)
import WAGS.Universe.Node (NodeC)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class GetSkolemizedFunctionFromAU, class LookupSkolem, class MakeInternalSkolemStack, class SkolemNotYetPresent, class SkolemNotYetPresentOrDiscardable, class ToSkolemizedFunction, DiscardableSkolem, SkolemListCons, SkolemPairC, getSkolemizedFunctionFromAU)
import WAGS.Universe.Universe (Universe, UniverseC)

-- | Create audio units using template `a` for input universe `i`, resulting in output universe `o` as well as a reference to the top-level created audio unit(s). The example below creates a speaker, a gain unit and two sine-wave oscillators.  The gain is connected to the speaker and the sine wave oscillators are connected to the gain.
-- |
-- | ```purescript
-- | create (Speaker (Gain 1.0 (SinOsc 440.0 /\ SinOsc 330.0 /\ Unit)))
-- | ```
-- |
-- | Feedback loops are possible using proxies.  The example below loops a gain unit into itself with a 
-- | delay of 0.2 seconds, creating an echo effect.
-- |
-- | ```purescript
-- | data MyGain
-- | myCreate =
-- |   create (Speaker (Gain 1.0 \(myGain :: Proxy MyGain) ->
-- |     (PlayBuf (Proxy :: _ "hello")
-- |       /\ Highpass 440.0 1.0 (Delay 0.2 (Gain 0.5 myGain))
-- |       /\ Unit)))
-- | ```
-- |
-- | Created audio units do not have to have a `Speaker` at the top-level. It is possible to create
-- | an audio unit and then connect it to another one using `connect`.
class Create (a :: Type) (i :: Universe) (o :: Universe) (ref :: Type) | a i -> o ref where
  create :: forall env audio engine proof m. Monad m => AudioInterpret audio engine => a -> FrameT env audio engine proof m i o ref

-- | A value that can be coerced to an initial control-rate audio parameter.
class InitialVal a where
  initialVal :: a -> AudioParameter

instance initialValNumber :: InitialVal Number where
  initialVal a = AudioParameter $ defaultParam { param = a }

instance initialValAudioParameter :: InitialVal AudioParameter where
  initialVal = identity

instance initialValTuple :: InitialVal a => InitialVal (Tuple a b) where
  initialVal = initialVal <<< fst



-- | Internal class used to make term-level instructions for audio unit creation.
class
  AudioInterpret audio engine <= CreationInstructions (audio :: Type) (engine :: Type) (g :: Type) where
  creationInstructions :: Int -> g -> Array (audio -> engine) /\ AnAudioUnit

instance creationInstructionsAllpass :: (AudioInterpret audio engine, InitialVal argA, InitialVal argB) => CreationInstructions audio engine (CTOR.Allpass argA argB argC) where
  creationInstructions idx (CTOR.Allpass argA argB _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB
    in
      [ makeAllpass idx argA_iv' argB_iv' ]
        /\ AAllpass argA_iv' argB_iv'

instance creationInstructionsBandpass ::
  ( AudioInterpret audio engine
  , InitialVal argA
  , InitialVal argB
  ) =>
  CreationInstructions audio engine (CTOR.Bandpass argA argB argC) where
  creationInstructions idx (CTOR.Bandpass argA argB _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB
    in
      [ makeBandpass idx argA_iv' argB_iv' ]
        /\ ABandpass argA_iv' argB_iv'

instance creationInstructionsConstant :: (AudioInterpret audio engine, InitialVal argA) => CreationInstructions audio engine (CTOR.Constant argA) where
  creationInstructions idx (CTOR.Constant onOff argA) =
    let
      argA_iv' = initialVal argA
    in
      [ makeConstant idx onOff argA_iv' ]
        /\ AConstant onOff argA_iv'

instance creationInstructionsConvolver :: (IsSymbol argA, AudioInterpret audio engine) => CreationInstructions audio engine (CTOR.Convolver argA argB) where
  creationInstructions idx (CTOR.Convolver px _) = let name = (reflectSymbol px) in [ makeConvolver idx name ] /\ AConvolver name

instance creationInstructionsDelay :: (AudioInterpret audio engine, InitialVal argA) => CreationInstructions audio engine (CTOR.Delay argA argB) where
  creationInstructions idx (CTOR.Delay argA _) =
    let
      argA_iv' = initialVal argA
    in
      [ makeDelay idx argA_iv' ]
        /\ ADelay argA_iv'

instance creationInstructionsDynamicsCompressor :: (AudioInterpret audio engine, InitialVal argA, InitialVal argB, InitialVal argC, InitialVal argD, InitialVal argE) => CreationInstructions audio engine (CTOR.DynamicsCompressor argA argB argC argD argE argF) where
  creationInstructions idx (CTOR.DynamicsCompressor argA argB argC argD argE _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB

      argC_iv' = initialVal argC

      argD_iv' = initialVal argD

      argE_iv' = initialVal argE
    in
      [ makeDynamicsCompressor idx argA_iv' argB_iv' argC_iv' argD_iv' argE_iv' ]
        /\ ADynamicsCompressor argA_iv' argB_iv' argC_iv' argD_iv' argE_iv'

instance creationInstructionsGain :: (AudioInterpret audio engine, InitialVal argA) => CreationInstructions audio engine (CTOR.Gain argA argB) where
  creationInstructions idx (CTOR.Gain argA _) =
    let
      argA_iv' = initialVal argA
    in
      [ makeGain idx argA_iv' ]
        /\ AGain argA_iv'

instance creationInstructionsHighpass :: (AudioInterpret audio engine, InitialVal argA, InitialVal argB) => CreationInstructions audio engine (CTOR.Highpass argA argB argC) where
  creationInstructions idx (CTOR.Highpass argA argB _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB
    in
      [ makeHighpass idx argA_iv' argB_iv' ]
        /\ AHighpass argA_iv' argB_iv'

instance creationInstructionsHighshelf :: (AudioInterpret audio engine, InitialVal argA, InitialVal argB) => CreationInstructions audio engine (CTOR.Highshelf argA argB argC) where
  creationInstructions idx (CTOR.Highshelf argA argB _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB
    in
      [ makeHighshelf idx argA_iv' argB_iv' ]
        /\ AHighshelf argA_iv' argB_iv'

instance creationInstructionsLoopBuf :: (IsSymbol argA, AudioInterpret audio engine, InitialVal argB) => CreationInstructions audio engine (CTOR.LoopBuf argA argB) where
  creationInstructions idx (CTOR.LoopBuf px onOff argB loopStart loopEnd) =
    let
      argB_iv' = initialVal argB

      bufname = reflectSymbol px
    in
      [ makeLoopBuf idx bufname onOff argB_iv' loopStart loopEnd ]
        /\ ALoopBuf bufname onOff argB_iv' loopStart loopEnd

instance creationInstructionsLowpass :: (AudioInterpret audio engine, InitialVal argA, InitialVal argB) => CreationInstructions audio engine (CTOR.Lowpass argA argB argC) where
  creationInstructions idx (CTOR.Lowpass argA argB _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB
    in
      [ makeLowpass idx argA_iv' argB_iv' ]
        /\ ALowpass argA_iv' argB_iv'

instance creationInstructionsLowshelf :: (AudioInterpret audio engine, InitialVal argA, InitialVal argB) => CreationInstructions audio engine (CTOR.Lowshelf argA argB argC) where
  creationInstructions idx (CTOR.Lowshelf argA argB _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB
    in
      [ makeLowshelf idx argA_iv' argB_iv' ]
        /\ ALowshelf argA_iv' argB_iv'

instance creationInstructionsMicrophone :: AudioInterpret audio engine => CreationInstructions audio engine (CTOR.Microphone) where
  creationInstructions idx _ = [ makeMicrophone idx ] /\ AMicrophone

instance creationInstructionsNotch :: (AudioInterpret audio engine, InitialVal argA, InitialVal argB) => CreationInstructions audio engine (CTOR.Notch argA argB argC) where
  creationInstructions idx (CTOR.Notch argA argB _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB
    in
      [ makeNotch idx argA_iv' argB_iv' ]
        /\ ANotch argA_iv' argB_iv'

instance creationInstructionsPeaking :: (AudioInterpret audio engine, InitialVal argA, InitialVal argB, InitialVal argC) => CreationInstructions audio engine (CTOR.Peaking argA argB argC argD) where
  creationInstructions idx (CTOR.Peaking argA argB argC _) =
    let
      argA_iv' = initialVal argA

      argB_iv' = initialVal argB

      argC_iv' = initialVal argC
    in
      [ makePeaking idx argA_iv' argB_iv' argC_iv' ]
        /\ APeaking argA_iv' argB_iv' argC_iv'

instance creationInstructionsPeriodicOsc :: (IsSymbol argA, AudioInterpret audio engine, InitialVal argB) => CreationInstructions audio engine (CTOR.PeriodicOsc argA argB) where
  creationInstructions idx (CTOR.PeriodicOsc px onOff argB) =
    let
      argB_iv' = initialVal argB

      oscname = reflectSymbol px
    in
      [ makePeriodicOsc idx oscname onOff argB_iv' ]
        /\ APeriodicOsc oscname onOff argB_iv'

instance creationInstructionsPlayBuf :: (IsSymbol argA, AudioInterpret audio engine, InitialVal argB) => CreationInstructions audio engine (CTOR.PlayBuf argA argB) where
  creationInstructions idx (CTOR.PlayBuf px offset onOff argB) =
    let
      argB_iv' = initialVal argB

      bufname = reflectSymbol px
    in
      [ makePlayBuf idx bufname offset onOff argB_iv' ]
        /\ APlayBuf bufname offset onOff argB_iv'

instance creationInstructionsRecorder :: (IsSymbol argA, AudioInterpret audio engine) => CreationInstructions audio engine (CTOR.Recorder argA argB) where
  creationInstructions idx (CTOR.Recorder px _) = let name = (reflectSymbol px) in [ makeRecorder idx name ] /\ ARecorder name

instance creationInstructionsSawtoothOsc :: (AudioInterpret audio engine, InitialVal argA) => CreationInstructions audio engine (CTOR.SawtoothOsc argA) where
  creationInstructions idx (CTOR.SawtoothOsc onOff argA) =
    let
      argA_iv' = initialVal argA
    in
      [ makeSawtoothOsc idx onOff argA_iv' ]
        /\ ASawtoothOsc onOff argA_iv'

instance creationInstructionsSinOsc :: (AudioInterpret audio engine, InitialVal argA) => CreationInstructions audio engine (CTOR.SinOsc argA) where
  creationInstructions idx (CTOR.SinOsc onOff argA) =
    let
      argA_iv' = initialVal argA
    in
      [ makeSinOsc idx onOff argA_iv' ]
        /\ ASinOsc onOff argA_iv'

instance creationInstructionsSpeaker :: AudioInterpret audio engine => CreationInstructions audio engine (CTOR.Speaker argA) where
  creationInstructions idx (CTOR.Speaker _) = [ makeSpeaker idx ] /\ ASpeaker

instance creationInstructionsSquareOsc :: (AudioInterpret audio engine, InitialVal argA) => CreationInstructions audio engine (CTOR.SquareOsc argA) where
  creationInstructions idx (CTOR.SquareOsc onOff argA) =
    let
      argA_iv' = initialVal argA
    in
      [ makeSquareOsc idx onOff argA_iv' ]
        /\ ASquareOsc onOff argA_iv'

instance creationInstructionsStereoPanner :: (AudioInterpret audio engine, InitialVal argA) => CreationInstructions audio engine (CTOR.StereoPanner argA argB) where
  creationInstructions idx (CTOR.StereoPanner argA _) =
    let
      argA_iv' = initialVal argA
    in
      [ makeStereoPanner idx argA_iv' ]
        /\ AStereoPanner argA_iv'

instance creationInstructionsTriangleOsc :: (AudioInterpret audio engine, InitialVal argA) => CreationInstructions audio engine (CTOR.TriangleOsc argA) where
  creationInstructions idx (CTOR.TriangleOsc onOff argA) =
    let
      argA_iv' = initialVal argA
    in
      [ makeTriangleOsc idx onOff argA_iv' ]
        /\ ATriangleOsc onOff argA_iv'

instance creationInstructionsWaveShaper :: (IsSymbol argA, AudioInterpret audio engine, IsOversample argB) => CreationInstructions audio engine (CTOR.WaveShaper argA argB argC) where
  creationInstructions idx (CTOR.WaveShaper argA argB _) =
    let
      name = (reflectSymbol argA)

      os = (reflectOversample argB)
    in
      [ makeWaveShaper idx name os ] /\ AWaveShaper name os

creationStep ::
  forall env audio engine proof m g.
  Monad m =>
  AudioInterpret audio engine =>
  CreationInstructions audio engine g =>
  g ->
  AudioState env audio engine proof m Int
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
  forall env audio engine proof g (ptr :: BinL) skolem c (i :: Universe) (o :: Universe) innerTerm eprof m.
  Monad m =>
  AudioInterpret audio engine =>
  GetSkolemizedFunctionFromAU g skolem c =>
  AsEdgeProfile innerTerm eprof =>
  CreationInstructions audio engine g =>
  Create c i o innerTerm =>
  Proxy (skolem /\ (Proxy ptr) /\ innerTerm /\ (Proxy i) /\ (Proxy o)) ->
  g ->
  FrameT env audio engine proof m i o Int
createAndConnect _ g =
  unsafeFrame
    $ do
        idx <- cs
        let
          mc =
            unsafeUnframe
              $ (create :: forall mo. Monad mo => c -> FrameT env audio engine proof mo i o innerTerm)
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
                    <> map (flip connectXToY idx) o
                }
          )
        pure idx
  where
  cs = creationStep g

-- end of the line in tuples
instance createUnit ::
  Create Unit u u Unit where
  create = unsafeFrame <<< pure

instance createTuple ::
  (Create x u0 u1 x', Create y u1 u2 y') =>
  Create (x /\ y) u0 u2 (x' /\ y') where
  create (x /\ y) = (unsafeFrame) $ Tuple <$> x' <*> y'
    where
    (x') = unsafeUnframe $ (create :: forall env audio engine proof m. Monad m => AudioInterpret audio engine => x -> FrameT env audio engine proof m u0 u1 x') x

    (y') = unsafeUnframe $ (create :: forall env audio engine proof m. Monad m => AudioInterpret audio engine => y -> FrameT env audio engine proof m u1 u2 y') y

instance createIdentity :: Create x i o r => Create (Identity x) i o r where
  create (Identity x) = create x

instance createFocus :: Create x i o r => Create (Focus x) i o r where
  create (Focus x) = create x

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
  create (Dup a f) = unsafeFrame $ x *> y
    where
    x =
      unsafeUnframe
        $ ( create ::
              forall env audio engine proof m.
              Monad m =>
              AudioInterpret audio engine =>
              a ->
              FrameT env audio engine proof m
                (UniverseC ptr graphi changeBit skolems)
                (UniverseC midptr graphm changeBit skolems)
                ignore
          )
            a

    y =
      unsafeUnframe
        $ ( create ::
              forall env audio engine proof m.
              Monad m =>
              AudioInterpret audio engine =>
              b ->
              FrameT env audio engine proof m
                (UniverseC midptr graphm changeBit (SkolemListCons (SkolemPairC skolem ptr) skolems))
                (UniverseC outptr grapho changeBit (SkolemListCons (SkolemPairC skolem ptr) skolems))
                (AudioUnitRef midptr)
          )
            (f (Proxy :: _ skolem))

instance createAllpass ::
  ( InitialVal argA
  , InitialVal argB
  , GetSkolemFromRecursiveArgument fOfargC skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createBandpass ::
  ( InitialVal argA
  , InitialVal argB
  , GetSkolemFromRecursiveArgument fOfargC skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createConstant ::
  ( InitialVal argA
  , BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createConvolver ::
  ( IsSymbol argA
  , GetSkolemFromRecursiveArgument fOfargB skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createDelay ::
  ( InitialVal argA
  , GetSkolemFromRecursiveArgument fOfargB skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createDynamicsCompressor ::
  ( InitialVal argA
  , InitialVal argB
  , InitialVal argC
  , InitialVal argD
  , InitialVal argE
  , GetSkolemFromRecursiveArgument fOfargF skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createHighpass ::
  ( InitialVal argA
  , InitialVal argB
  , GetSkolemFromRecursiveArgument fOfargC skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createHighshelf ::
  ( InitialVal argA
  , InitialVal argB
  , GetSkolemFromRecursiveArgument fOfargC skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createLoopBuf ::
  ( InitialVal argB
  , BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createLowpass ::
  ( InitialVal argA
  , InitialVal argB
  , GetSkolemFromRecursiveArgument fOfargC skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createLowshelf ::
  ( InitialVal argA
  , InitialVal argB
  , GetSkolemFromRecursiveArgument fOfargC skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createMicrophone ::
  ( BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createNotch ::
  ( InitialVal argA
  , InitialVal argB
  , GetSkolemFromRecursiveArgument fOfargC skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createPeaking ::
  ( InitialVal argA
  , InitialVal argB
  , InitialVal argC
  , GetSkolemFromRecursiveArgument fOfargD skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createPeriodicOsc ::
  ( InitialVal argB
  , BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createPlayBuf ::
  ( InitialVal argB
  , BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createRecorder ::
  ( IsSymbol argA
  , GetSkolemFromRecursiveArgument fOfargB skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createSawtoothOsc ::
  ( InitialVal argA
  , BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createSinOsc ::
  ( InitialVal argA
  , BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createSquareOsc ::
  ( InitialVal argA
  , BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createStereoPanner ::
  ( InitialVal argA
  , GetSkolemFromRecursiveArgument fOfargB skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolemsInternal)) (Proxy (UniverseC outptr grapho changeBit skolemsInternal)))

instance createTriangleOsc ::
  ( InitialVal argA
  , BinToInt ptr
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
  create = unsafeFrame <<< (map) AudioUnitRef <<< creationStep

instance createWaveShaper ::
  ( IsSymbol argA
  , IsOversample argB
  , GetSkolemFromRecursiveArgument fOfargC skolem
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
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
    unsafeFrame <<< (map) AudioUnitRef <<< unsafeUnframe
      <<< (createAndConnect (Proxy :: ProxyCC DiscardableSkolem (Proxy ptr) term (Proxy (UniverseC next graphi changeBit skolems)) (Proxy (UniverseC outptr grapho changeBit skolems))))

----------
instance createProxy ::
  ( LookupSkolem skolem skolems ptr
  , BinToInt ptr
  ) =>
  Create
    (Proxy skolem)
    (UniverseC next graph changeBit skolems)
    (UniverseC next graph changeBit skolems)
    (AudioUnitRef ptr) where
  create _ = unsafeFrame (pure $ AudioUnitRef $ toInt' (Proxy :: Proxy ptr))
