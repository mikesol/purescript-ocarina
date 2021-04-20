module Test.Rebase where

import Type.Data.Peano (Z)
import WAGS.Universe.AudioUnit (TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4, D5, D6, D7, D8, D9)
import WAGS.Universe.Bin (PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (UniverseC)

data MyGain

data MySinOsc

type RBL
  =  UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        Z SkolemListNil
    

type RBR
  = UniverseC D9
        ( GraphC
            (NodeC (TSpeaker D5) (SingleEdge D7))
            ( NodeListCons
                ( NodeC (TGain D7)
                    (ManyEdges D7 (PtrListCons D8 (PtrListCons D6 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D8) (SingleEdge D6))
                    (NodeListCons (NodeC (TSinOsc D6) NoEdge) NodeListNil)
                )
            )
        )
        Z SkolemListNil

{-
-- internal compiler error
rebaseTest0 ::
  Frame Unit Void RBL RBR Unit
rebaseTest0 = rebase (Proxy :: _ RBL) (Proxy :: _ RBR)
-}