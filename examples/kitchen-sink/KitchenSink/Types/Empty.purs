module WAGS.Example.KitchenSink.Types.Empty where

import Type.Proxy (Proxy(..))
import WAGS.Rebase (ResetSig)
import WAGS.Rebase as Rb
import WAGS.Universe.AudioUnit (TGain, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

type EI
  = D2

type EI0
  = EI

type EI1
  = D3

type EI2
  = D4

type EmptyGraph
  = GraphC
      (NodeC (TSpeaker D0) (SingleEdge D1))
      (NodeListCons (NodeC (TGain D1) NoEdge) NodeListNil)

type BaseGraph ptr
  = NodeListCons
      (NodeC (TSpeaker D0) (SingleEdge D1))
      (NodeListCons (NodeC (TGain D1) (SingleEdge ptr)) NodeListNil)

reset :: ResetSig EI EmptyGraph
reset = Rb.reset (Proxy :: _ EI) (Proxy :: _ EmptyGraph)
