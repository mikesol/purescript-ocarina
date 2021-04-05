module WAGS.Create where

import Prelude

import Control.Monad.State (gets, modify_)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Set as S
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Frame(..), AudioState)
import WAGS.Graph.Constructors (Dup(..), Gain(..), Highpass(..), SinOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..))
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam)
import WAGS.Rendered (AnAudioUnit(..), Instruction(..))
import WAGS.Universe.AudioUnit (AudioUnitRef(..), TGain, THighpass, TSinOsc, TSpeaker)
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

class Create (a :: Type) (i :: Universe) (o :: Universe) (x :: Type) | a i -> o x where
  create :: forall env proof. a -> Frame env proof i o x

creationStep ::
  forall env g.
  CreationInstructions g =>
  g ->
  AudioState env Int
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
    (UniverseC next graph skolems)
    (UniverseC next graph skolems)
    (AudioUnitRef ptr) where
  create _ = Frame $ (pure $ AudioUnitRef $ toInt' (Proxy :: Proxy ptr))

instance createDup ::
  ( SkolemNotYetPresent skolem skolems
  , BinToInt ptr
  , Create
      a
      (UniverseC ptr graphi skolems)
      (UniverseC midptr graphm skolems)
      ignore
  , Create
      b
      (UniverseC midptr graphm (SkolemListCons (SkolemPairC skolem ptr) skolems))
      (UniverseC outptr grapho (SkolemListCons (SkolemPairC skolem ptr) skolems))
      (AudioUnitRef midptr)
  ) =>
  Create
    (Dup a (Proxy skolem -> b))
    (UniverseC ptr graphi skolems)
    (UniverseC outptr grapho skolems)
    (AudioUnitRef midptr) where
  create (Dup a f) = Frame $ x *> y
    where
    Frame x =
      ( create ::
          forall env proof.
          a ->
          Frame env proof
            (UniverseC ptr graphi skolems)
            (UniverseC midptr graphm skolems)
            ignore
      )
        a

    Frame y =
      ( create ::
          forall env proof.
          b ->
          Frame env proof
            (UniverseC midptr graphm (SkolemListCons (SkolemPairC skolem ptr) skolems))
            (UniverseC outptr grapho (SkolemListCons (SkolemPairC skolem ptr) skolems))
            (AudioUnitRef midptr)
      )
        (f (Proxy :: _ skolem))

instance createSinOsc ::
  ( InitialVal a
  , BinToInt ptr
  , BinSucc ptr next
  , GraphToNodeList graph nodeList
  ) =>
  Create
    (SinOsc a)
    (UniverseC ptr graph skolems)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr) NoEdge) nodeList)
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
      (UniverseC next graphi skolemsInternal)
      (UniverseC outptr grapho skolemsInternal)
      term
  , AsEdgeProfile term (SingleEdge op)
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Highpass a b fc)
    (UniverseC ptr graphi skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (THighpass ptr) (SingleEdge op)) nodeList)
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi skolemsInternal)) (Proxy (UniverseC outptr grapho skolemsInternal)))

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
      (UniverseC next graphi skolemsInternal)
      (UniverseC outptr grapho skolemsInternal)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Gain a fb)
    (UniverseC ptr graphi skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (TGain ptr) eprof) nodeList)
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< (createAndConnect (Proxy :: ProxyCC skolem (Proxy ptr) term (Proxy (UniverseC next graphi skolemsInternal)) (Proxy (UniverseC outptr grapho skolemsInternal))))

instance createSpeaker ::
  ( ToSkolemizedFunction a DiscardableSkolem a
  , BinToInt ptr
  , BinSucc ptr next
  , Create
      a
      (UniverseC next graphi skolems)
      (UniverseC outptr grapho skolems)
      term
  , AsEdgeProfile term eprof
  , GraphToNodeList grapho nodeList
  ) =>
  Create
    (Speaker a)
    (UniverseC ptr graphi skolems)
    ( UniverseC
        outptr
        (GraphC (NodeC (TSpeaker ptr) eprof) nodeList)
        skolems
    )
    (AudioUnitRef ptr) where
  create =
    Frame <<< (map) AudioUnitRef <<< (\(Frame x) -> x)
      <<< (createAndConnect (Proxy :: ProxyCC DiscardableSkolem (Proxy ptr) term (Proxy (UniverseC next graphi skolems)) (Proxy (UniverseC outptr grapho skolems))))

----------
