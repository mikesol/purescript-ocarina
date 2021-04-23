module WAGS.Example.KitchenSink.Types.Empty where

import Type.Proxy (Proxy(..))
import WAGS.Graph.Constructors (Gain, Speaker)
import WAGS.Graph.Optionals (GetSetAP)
import WAGS.Rebase (ResetSig)
import WAGS.Rebase as Rb
import WAGS.Universe.AudioUnit (TGain, TSpeaker)
import WAGS.Universe.BinN (D0, D1, D2, D3, D4, D5, D6)
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

type EI3
  = D5

type EI4
  = D6

type EmptyGraph
  = GraphC
      (NodeC (TSpeaker D0) (SingleEdge D1))
      (NodeListCons (NodeC (TGain D1) NoEdge) NodeListNil)

type BaseGraph ptr
  = NodeListCons
      (NodeC (TSpeaker D0) (SingleEdge D1))
      (NodeListCons (NodeC (TGain D1) (SingleEdge ptr)) NodeListNil)

type TopLevel g c
  = Speaker (g (Gain GetSetAP c))


reset :: ResetSig EI EmptyGraph
reset = Rb.reset (Proxy :: _ EI) (Proxy :: _ EmptyGraph)
