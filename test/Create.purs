module Test.WAGS.Create where

import Prelude
import Data.Tuple.Nested ((/\))
import Type.Data.Peano (Z)
import Type.Proxy (Proxy)
import WAGS as W

data MyGain

data MySinOsc

createTest1 ::
  forall ptr next env skolems head tail proof.
  W.BinToInt ptr =>
  W.BinToInt next =>
  W.BinSucc ptr next =>
  W.Frame env proof (W.UniverseC ptr (W.GraphC head tail) Z skolems)
    ( W.UniverseC next
        (W.GraphC (W.NodeC (W.TSinOsc ptr) W.NoEdge) (W.NodeListCons head tail))
        Z
        skolems
    )
    (W.AudioUnitRef ptr)
createTest1 = W.create (W.SinOsc 440.0)

createTest2 ::
  forall first mid last env skolems head tail proof.
  W.BinToInt first =>
  W.BinToInt mid =>
  W.BinToInt last =>
  W.BinSucc first mid =>
  W.BinSucc mid last =>
  W.Frame env proof (W.UniverseC first (W.GraphC head tail) Z skolems)
    ( W.UniverseC last
        (W.GraphC (W.NodeC (W.THighpass first) (W.SingleEdge mid)) (W.NodeListCons (W.NodeC (W.TSinOsc mid) W.NoEdge) (W.NodeListCons head tail)))
        Z
        skolems
    )
    (W.AudioUnitRef first)
createTest2 = W.create (W.Highpass 440.0 1.0 (W.SinOsc 440.0))

createTest3 ::
  forall first mid last env head tail proof.
  W.BinToInt first =>
  W.BinToInt mid =>
  W.BinToInt last =>
  W.BinSucc first mid =>
  W.BinSucc mid last =>
  W.Frame env proof (W.UniverseC first (W.GraphC head tail) Z W.SkolemListNil)
    ( W.UniverseC last
        ( W.GraphC
            ( W.NodeC (W.TGain first)
                (W.ManyEdges first (W.PtrListCons mid W.PtrListNil))
            )
            (W.NodeListCons (W.NodeC (W.TSinOsc mid) W.NoEdge) (W.NodeListCons head tail))
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef first)
createTest3 = W.create (W.Gain 1.0 (\(gain :: Proxy MyGain) -> gain /\ W.SinOsc 440.0 /\ unit))

createTest4 ::
  forall first mid0 mid1 last env head tail proof.
  W.BinToInt first =>
  W.BinToInt mid0 =>
  W.BinToInt mid1 =>
  W.BinToInt last =>
  W.BinSucc first mid0 =>
  W.BinSucc mid0 mid1 =>
  W.BinSucc mid1 last =>
  W.Frame env proof (W.UniverseC first (W.GraphC head tail) Z W.SkolemListNil)
    ( W.UniverseC last
        ( W.GraphC
            ( W.NodeC (W.TGain first)
                (W.ManyEdges first (W.PtrListCons mid0 W.PtrListNil))
            )
            ( W.NodeListCons (W.NodeC (W.THighpass mid0) (W.SingleEdge mid1))
                (W.NodeListCons (W.NodeC (W.TSinOsc mid1) W.NoEdge) (W.NodeListCons head tail))
            )
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef first)
createTest4 =
  W.create
    $ W.Gain 1.0 \(gain :: Proxy MyGain) ->
        gain /\ W.Highpass 330.0 1.0 (W.SinOsc 440.0) /\ unit

createTest5 ::
  forall env head tail proof.
  W.Frame env proof (W.UniverseC W.D0 (W.GraphC head tail) Z W.SkolemListNil)
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) (W.NodeListCons head tail))
            )
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D1)
createTest5 =
  W.create
    $ W.Dup (W.SinOsc 440.0) \(mySinOsc :: Proxy MySinOsc) ->
        W.Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ W.Highpass 330.0 1.0 mySinOsc /\ mySinOsc /\ unit
