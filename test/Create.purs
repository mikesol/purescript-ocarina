module Test.GS.Create where

import Prelude

import Data.Tuple.Nested ((/\))
import Type.Data.Peano (Z)
import Type.Proxy (Proxy)
import WAGS.Control.Types (Frame)
import WAGS.Create (create)
import WAGS.Graph.Constructors (Dup(..), Gain(..), Highpass(..), OnOff(..), SinOsc(..))
import WAGS.Interpret (class AudioInterpret)
import WAGS.Universe.AudioUnit (AudioUnitRef, TGain, THighpass, TSinOsc)
import WAGS.Universe.BinN (D0, D1, D2, D3)
import WAGS.Universe.Bin (class BinSucc, class BinToInt, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (UniverseC)

data MyGain

data MySinOsc

createTest1 ::
  forall ptr next env audio engine skolems head tail proof.
  BinToInt ptr =>
  BinToInt next =>
  BinSucc ptr next =>
  AudioInterpret audio engine =>
  Frame env audio engine proof (UniverseC ptr (GraphC head tail) Z skolems)
    ( UniverseC next
        (GraphC (NodeC (TSinOsc ptr) NoEdge) (NodeListCons head tail))
        Z
        skolems
    )
    (AudioUnitRef ptr)
createTest1 = create (SinOsc On 440.0)

createTest2 ::
  forall first mid last env audio engine skolems head tail proof.
  BinToInt first =>
  BinToInt mid =>
  BinToInt last =>
  BinSucc first mid =>
  BinSucc mid last =>
  AudioInterpret audio engine =>
  Frame env audio engine proof (UniverseC first (GraphC head tail) Z skolems)
    ( UniverseC last
        (GraphC (NodeC (THighpass first) (SingleEdge mid)) (NodeListCons (NodeC (TSinOsc mid) NoEdge) (NodeListCons head tail)))
        Z
        skolems
    )
    (AudioUnitRef first)
createTest2 = create (Highpass 440.0 1.0 (SinOsc On 440.0))

createTest3 ::
  forall first mid last env audio engine head tail proof.
  BinToInt first =>
  BinToInt mid =>
  BinToInt last =>
  BinSucc first mid =>
  BinSucc mid last =>
  AudioInterpret audio engine =>
  Frame env audio engine proof (UniverseC first (GraphC head tail) Z SkolemListNil)
    ( UniverseC last
        ( GraphC
            ( NodeC (TGain first)
                (ManyEdges first (PtrListCons mid PtrListNil))
            )
            (NodeListCons (NodeC (TSinOsc mid) NoEdge) (NodeListCons head tail))
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef first)
createTest3 = create (Gain 1.0 (\(gain :: Proxy MyGain) -> gain /\ SinOsc On 440.0 /\ unit))

createTest4 ::
  forall first mid0 mid1 last env audio engine head tail proof.
  BinToInt first =>
  BinToInt mid0 =>
  BinToInt mid1 =>
  BinToInt last =>
  BinSucc first mid0 =>
  BinSucc mid0 mid1 =>
  BinSucc mid1 last =>
  AudioInterpret audio engine =>
  Frame env audio engine proof (UniverseC first (GraphC head tail) Z SkolemListNil)
    ( UniverseC last
        ( GraphC
            ( NodeC (TGain first)
                (ManyEdges first (PtrListCons mid0 PtrListNil))
            )
            ( NodeListCons (NodeC (THighpass mid0) (SingleEdge mid1))
                (NodeListCons (NodeC (TSinOsc mid1) NoEdge) (NodeListCons head tail))
            )
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef first)
createTest4 =
  create
    $ Gain 1.0 \(gain :: Proxy MyGain) ->
        gain /\ Highpass 330.0 1.0 (SinOsc On 440.0) /\ unit

createTest5 ::
  forall env audio engine head tail proof.
  AudioInterpret audio engine =>
  Frame env audio engine proof (UniverseC D0 (GraphC head tail) Z SkolemListNil)
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) (NodeListCons head tail))
            )
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef D1)
createTest5 =
  create
    $ Dup (SinOsc On 440.0) \(mySinOsc :: Proxy MySinOsc) ->
        Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ Highpass 330.0 1.0 mySinOsc /\ mySinOsc /\ unit
