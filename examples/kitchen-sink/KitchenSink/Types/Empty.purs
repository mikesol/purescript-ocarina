module WAGS.Example.KitchenSink.Types.Empty where

import Prelude

import Effect (Effect)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (currentIdx, graph)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame)
import WAGS.Interpret (FFIAudio)
import WAGS.Rebase (class Rebase', RebaseProof, rebase)
import WAGS.Run (SceneI)
import WAGS.Universe.AudioUnit (TGain, TSpeaker)
import WAGS.Universe.Bin (PtrListNil)
import WAGS.Universe.BinN (D0, D1, D2)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Validation (class TerminalIdentityEdge)

type EI
  = D2

type EmptyGraph
  = GraphC
      (NodeC (TSpeaker D0) (SingleEdge D1))
      (NodeListCons (NodeC (TGain D1) NoEdge) NodeListNil)

reset :: forall proof cb e0 i0 g0 e1.
  TerminalIdentityEdge g0 e0 =>
  TerminalIdentityEdge EmptyGraph e1 =>
  Rebase' PtrListNil PtrListNil RebaseProof e0 i0 g0 e1 EI EmptyGraph =>
  Frame (SceneI Unit Unit) FFIAudio (Effect Unit) proof (UniverseC i0 g0 cb SkolemListNil) (UniverseC EI EmptyGraph cb SkolemListNil) Unit
reset = WAGS.do
  ci <- currentIdx
  g <- graph
  rebase ci g (Proxy :: _ EI) (Proxy :: _ EmptyGraph)
