module Test.Move where

import Prelude
import Type.Data.Peano (Succ, Z)
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Frame)
import WAGS.Move (move)
import WAGS.MoveNode (moveNode)
import WAGS.Universe.AudioUnit (AudioUnitRef, TGain, THighpass, TSinOsc)
import WAGS.Universe.Bin (PtrListCons, PtrListNil)
import WAGS.Universe.BinN (D0, D1, D2, D3)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (UniverseC)

data MyGain

data MySinOsc

moveTest0 ::
  forall audio engine.
  AudioUnitRef D1 ->
  Frame Unit audio engine Void
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    Unit
moveTest0 ref = move ref (Proxy :: _ Z) (Proxy :: _ Z)

moveTest1 ::
  forall audio engine.
  AudioUnitRef D1 ->
  Frame Unit audio engine Void
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    Unit
moveTest1 ref = move ref ref (Proxy :: _ Z)

moveTest2 ::
  forall audio engine.
  AudioUnitRef D1 ->
  Frame Unit audio engine Void
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D2 (PtrListCons D1 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    Unit
moveTest2 ref = move ref ref (Proxy :: _ (Succ Z))

moveTest3 ::
  forall audio engine.
  AudioUnitRef D1 ->
  Frame Unit audio engine Void
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D2 (PtrListCons D0 (PtrListCons D1 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    Unit
moveTest3 ref = move ref ref (Proxy :: _ (Succ (Succ Z)))

moveTest4 ::
  forall audio engine.
  AudioUnitRef D1 ->
  Frame Unit audio engine Void
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D2 (PtrListCons D1 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    Unit
moveTest4 ref = move ref (Proxy :: _ (Succ Z)) (Proxy :: _ Z)

moveTest5 ::
  forall audio engine.
  AudioUnitRef D1 ->
  AudioUnitRef D2 ->
  Frame Unit audio engine Void
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D2 (PtrListCons D1 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    Unit
moveTest5 ref ref2 = move ref ref2 (Proxy :: _ Z)

moveTest6 ::
  forall audio engine.
  Frame Unit audio engine Void
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    ( UniverseC D3
        ( GraphC
            (NodeC (THighpass D2) (SingleEdge D0))
            ( NodeListCons
                ( NodeC (TGain D1)
                    (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
                )
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    Unit
moveTest6 = moveNode (Proxy :: _ (Succ Z)) (Proxy :: _ Z)
