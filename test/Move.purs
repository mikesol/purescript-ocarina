module Test.Move where

import Prelude
import Type.Data.Peano (Succ, Z)
import Type.Proxy (Proxy(..))
import WAGS as W

data MyGain

data MySinOsc

moveTest0 :: forall audio engine.
  W.AudioUnitRef W.D1 ->
  W.Frame Unit audio engine Void
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
moveTest0 ref = W.move ref (Proxy :: _ Z) (Proxy :: _ Z)

moveTest1 :: forall audio engine.
  W.AudioUnitRef W.D1 ->
  W.Frame Unit audio engine Void
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
moveTest1 ref = W.move ref ref (Proxy :: _ Z)

moveTest2 :: forall audio engine.
  W.AudioUnitRef W.D1 ->
  W.Frame Unit  audio engine Void
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D2 (W.PtrListCons W.D1 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
moveTest2 ref = W.move ref ref (Proxy :: _ (Succ Z))

moveTest3 :: forall audio engine.
  W.AudioUnitRef W.D1 ->
  W.Frame Unit audio engine Void
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D2 (W.PtrListCons W.D0 (W.PtrListCons W.D1 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
moveTest3 ref = W.move ref ref (Proxy :: _ (Succ (Succ Z)))

moveTest4 :: forall audio engine.
  W.AudioUnitRef W.D1 ->
  W.Frame Unit audio engine Void
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D2 (W.PtrListCons W.D1 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
moveTest4 ref = W.move ref (Proxy :: _ (Succ Z)) (Proxy :: _ Z)

moveTest5 :: forall audio engine.
  W.AudioUnitRef W.D1 ->
  W.AudioUnitRef W.D2 ->
  W.Frame Unit audio engine Void
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D2 (W.PtrListCons W.D1 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
moveTest5 ref ref2 = W.move ref ref2 (Proxy :: _ Z)
