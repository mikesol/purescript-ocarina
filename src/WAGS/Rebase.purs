module WAGS.Rebase where

import Prelude

import Control.Monad.State (modify_)
import Prim.TypeError (class Warn, Text)
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Frame(..), AudioState)
import WAGS.Rendered (Instruction(..))
import WAGS.Universe.AudioUnit (AudioUnitRef, TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Universe.Bin (class BinToInt, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (class GetGraph, Universe)
import WAGS.Validation (class LookupNL, class TerminalIdentityEdge)


rebase ::
  forall edgeA iA edgeB iB env proof.
  Warn (Text "starting rebase") =>
  TerminalIdentityEdge iA edgeA =>
  Warn (Text "found tieA") =>
  TerminalIdentityEdge iB edgeB =>
  Warn (Text "found tieB") =>
  Rebase edgeA iA edgeB iB =>
  Proxy iA -> Proxy iB -> Frame env proof iA iB Unit
rebase iA iB = rebase' (Proxy :: _ edgeA) iA (Proxy :: _ edgeB) iB 

rebaseAt ::
  forall ptrA iA ptrB iB env proof.
  Rebase (SingleEdge ptrA) iA (SingleEdge ptrB) iB =>
  AudioUnitRef ptrA -> Proxy iA -> AudioUnitRef ptrB -> Proxy iB -> Frame env proof iA iB Unit
rebaseAt _ iA _ iB = rebase' (Proxy :: _ (SingleEdge ptrA)) iA (Proxy :: _ (SingleEdge ptrB)) iB

class Rebase :: forall k1. k1 -> Universe -> k1 -> Universe -> Constraint
class Rebase pA (iA :: Universe) pB (iB :: Universe) where
  rebase' :: forall env proof. Proxy pA -> Proxy iA -> Proxy pB -> Proxy iB -> Frame env proof iA iB Unit

rebaseAudioUnit ::
  forall env ptrA ptrB.
  BinToInt ptrA =>
  BinToInt ptrB =>
  Proxy ptrA ->
  Proxy ptrB ->
  AudioState env Unit
rebaseAudioUnit ptrA ptrB = if iA == iB then pure unit else modify_ \i -> i { instructions = i.instructions <> [Rebase iA iB] }
  where
  iA = toInt' ptrA
  iB = toInt' ptrB

instance rebaseNoEdge :: Rebase NoEdge iA NoEdge iB where
  rebase' _ _ _ _ = Frame $ pure unit

instance rebaseMany2 ::
  ( Rebase (SingleEdge pA) iA (SingleEdge pB) iB,
    Rebase (ManyEdges aA bA) iA (ManyEdges aB bB) iB
  ) =>
  Rebase (ManyEdges pA (PtrListCons aA bA)) iA (ManyEdges pB (PtrListCons aB bB)) iB  where
  rebase' _ iA _ iB = Frame (l *> r)
    where
    Frame l = rebase' (Proxy :: _ (SingleEdge pA)) (Proxy :: _ iA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ iB)
    Frame r = rebase' (Proxy :: _ (ManyEdges aA bA)) (Proxy :: _ iA) (Proxy :: _ (ManyEdges aB bB)) (Proxy :: _ iB)

instance rebaseMany1 ::
  ( Rebase (SingleEdge pA) iA (SingleEdge pB) iB
  ) =>
  Rebase (ManyEdges pA PtrListNil) iA (ManyEdges pB PtrListNil) iB  where
  rebase' _ iA _ iB = rebase' (Proxy :: _ (SingleEdge pA)) (Proxy :: _ iA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ iB)

instance rebaseSingleEdge ::
  ( GetGraph iA gA,
    GraphToNodeList gA nlA,
    LookupNL NodeListNil pA nlA (NodeListCons nA NodeListNil),
    GetGraph iB gB,
    GraphToNodeList gB nlB,
    LookupNL NodeListNil pB nlB (NodeListCons nB NodeListNil),
    Rebase nA iA nB iB
  ) =>
  Rebase (SingleEdge pA) iA (SingleEdge pB) iB  where
  rebase' _ iA _ iB = rebase' (Proxy :: _ nA) (Proxy :: _ iA) (Proxy :: _ nB) (Proxy :: _ iB)

instance rebaseSinOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase (NodeC (TSinOsc ptrA) NoEdge) iA (NodeC (TSinOsc ptrB) NoEdge) iB where
  rebase' _ _ _ _ = Frame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseHighpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase eA iA eB iB) =>
  Rebase (NodeC (THighpass ptrA) eA) iA (NodeC (THighpass ptrB) eB) iB where
  rebase' _ _ _ _ = Frame $ do
    rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
    rest
    where
    Frame rest = rebase' (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseGain ::
  (BinToInt ptrA, BinToInt ptrB, Rebase eA iA eB iB) =>
  Rebase (NodeC (TGain ptrA) eA) iA (NodeC (TGain ptrB) eB) iB where
  rebase' _ _ _ _ = Frame $ do
    rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
    rest
    where
    Frame rest = rebase' (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseSpeaker ::
  (BinToInt ptrA, BinToInt ptrB, Rebase eA iA eB iB) =>
  Rebase (NodeC (TSpeaker ptrA) eA) iA (NodeC (TSpeaker ptrB) eB) iB where
  rebase' _ _ _ _ = Frame $ do
    rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
    rest
    where
    Frame rest = rebase' (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)
