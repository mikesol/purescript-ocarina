module Test.Rebase where

import Type.Data.Peano (Z)
import WAGS as W

data MyGain

data MySinOsc

type RBL
  =  W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D3 (W.PtrListCons W.D1 W.PtrListNil)))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D3) (W.SingleEdge W.D1))
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z W.SkolemListNil
    

type RBR
  = W.UniverseC W.D9
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D5) (W.SingleEdge W.D7))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D7)
                    (W.ManyEdges W.D7 (W.PtrListCons W.D8 (W.PtrListCons W.D6 W.PtrListNil)))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D8) (W.SingleEdge W.D6))
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D6) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z W.SkolemListNil

{-
-- internal compiler error
rebaseTest0 ::
  W.Frame Unit Void RBL RBR Unit
rebaseTest0 = W.rebase (Proxy :: _ RBL) (Proxy :: _ RBR)
-}