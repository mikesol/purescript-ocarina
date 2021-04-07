module WAGS.Rebase where

import Prelude

import Control.Monad.State (modify_)
import Data.Typelevel.Bool (False, True)
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Frame(..), AudioState)
import WAGS.Rendered (Instruction(..))
import WAGS.Universe.AudioUnit (TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Universe.Bin (class BinToInt, PtrList, PtrListCons, PtrListNil, Ptr, toInt')
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)
import WAGS.Universe.Universe (class GetGraph, Universe)
import WAGS.Validation (class LookupNL, class PtrInPtrList, class TerminalIdentityEdge)

data RebaseProof
  = RebaseProof

class Rebase :: Universe -> Universe -> Constraint
class Rebase iA iB where
  rebase :: forall env proof. Proxy iA -> Proxy iB -> Frame env proof iA iB Unit

instance rebaseRebase ::
  ( TerminalIdentityEdge graphA edgeA
  , TerminalIdentityEdge graphB edgeB
  , Rebase' PtrListNil PtrListNil RebaseProof edgeA (idxA graphA changeBitA skolemsA) edgeB (idxB graphB changeBitB skolemsB)
  ) =>
  Rebase (idxA graphA changeBitA skolemsA) (idxB graphB changeBitB skolemsB) where
  rebase iA iB = Frame do
    a <- arr
    modify_ \i -> i {
      instructions = i.instructions <> [Rebase a]
    }
    where
    Frame arr = rebase' (Proxy :: _ PtrListNil) (Proxy :: _ PtrListNil) RebaseProof (Proxy :: _ edgeA) iA (Proxy :: _ edgeB) iB

type AFT = (Array {from:: Int, to:: Int})
-- private, only used to collect rebase instructions
class Rebase' :: forall k1. PtrList -> PtrList -> Type -> k1 -> Universe -> k1 -> Universe -> Constraint
class Rebase' plA plB rbp pA (iA :: Universe) pB (iB :: Universe) where
  rebase' :: forall env proof. Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy iA -> Proxy pB -> Proxy iB -> Frame env proof iA iB AFT

class RebaseCheck' :: forall k1. PtrList -> PtrList -> Type -> k1 -> Universe -> k1 -> Universe -> Constraint
class RebaseCheck' plA plB rbp pA (iA :: Universe) pB (iB :: Universe) where
  rebaseCheck' :: forall env proof. Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy iA -> Proxy pB -> Proxy iB -> Frame env proof iA iB AFT

class RebaseCont' :: forall k1. Type -> Type -> Ptr -> Ptr -> PtrList -> PtrList -> Type -> k1 -> Universe -> k1 -> Universe -> Constraint
class RebaseCont' foundA foundB ptrA ptrB plA plB rbp pA (iA :: Universe) pB (iB :: Universe) where
  rebaseCont' :: forall env proof. Proxy foundA -> Proxy foundB -> Proxy ptrA -> Proxy ptrB -> Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy iA -> Proxy pB -> Proxy iB -> Frame env proof iA iB AFT

rebaseAudioUnit ::
  forall env proof ptrA ptrB.
  BinToInt ptrA =>
  BinToInt ptrB =>
  Proxy ptrA ->
  Proxy ptrB ->
  AudioState proof env AFT
rebaseAudioUnit ptrA ptrB = if iA == iB then pure mempty else (pure <<< pure) {from:iA, to:iB}
  where
  iA = toInt' ptrA

  iB = toInt' ptrB

instance rebaseNoEdge :: Rebase' rblA rblB RebaseProof NoEdge iA NoEdge iB where
  rebase' _ _ _ _ _ _ _ = Frame $ pure mempty

instance rebaseMany2 ::
  ( Rebase' rblA rblB RebaseProof (SingleEdge pA) iA (SingleEdge pB) iB
  , Rebase' rblA rblB RebaseProof (ManyEdges aA bA) iA (ManyEdges aB bB) iB
  ) =>
  Rebase' rblA rblB RebaseProof (ManyEdges pA (PtrListCons aA bA)) iA (ManyEdges pB (PtrListCons aB bB)) iB where
  rebase' _ _ _ _ iA _ iB = Frame (append <$> l <*> r)
    where
    Frame l = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (SingleEdge pA)) (Proxy :: _ iA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ iB)

    Frame r = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (ManyEdges aA bA)) (Proxy :: _ iA) (Proxy :: _ (ManyEdges aB bB)) (Proxy :: _ iB)

instance rebaseMany1 ::
  ( Rebase' rblA rblB RebaseProof (SingleEdge pA) iA (SingleEdge pB) iB
    ) =>
  Rebase' rblA rblB RebaseProof (ManyEdges pA PtrListNil) iA (ManyEdges pB PtrListNil) iB where
  rebase' _ _ _ _ iA _ iB = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (SingleEdge pA)) (Proxy :: _ iA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ iB)

instance rebaseSingleEdge ::
  ( RebaseCheck' rblA rblB RebaseProof (SingleEdge pA) iA (SingleEdge pB) iB
  ) =>
  Rebase' rblA rblB RebaseProof (SingleEdge pA) iA (SingleEdge pB) iB where
  rebase' = rebaseCheck' 

instance rebaseCheckSingleEdge ::
  ( GetGraph iA gA
  , GraphToNodeList gA nlA
  , LookupNL NodeListNil pA nlA (NodeListCons (NodeC igA epA) NodeListNil)
  , GetGraph iB gB
  , GraphToNodeList gB nlB
  , LookupNL NodeListNil pB nlB (NodeListCons (NodeC igB epB) NodeListNil)
  , PtrInPtrList False pA rblA tfA
  , PtrInPtrList False pB rblB tfB
  , RebaseCont' tfA tfB pA pB rblA rblB RebaseProof epA iA epB iB
  ) =>
  RebaseCheck' rblA rblB RebaseProof (SingleEdge pA) iA (SingleEdge pB) iB where
  rebaseCheck' _ _ _ _ iA _ iB = rebaseCont' (Proxy :: _ tfA) (Proxy :: _ tfB) (Proxy :: _ pA) (Proxy :: _ pB) (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ epA) (Proxy :: _ iA) (Proxy :: _ epB) (Proxy :: _ iB)

instance rebaseContTT ::
  RebaseCont' True True ptrA ptrB rblA rblB RebaseProof a b c d where
  rebaseCont' _ _ _ _ _ _ _ _ _ _ _ = Frame $ pure mempty

instance rebaseContFF ::
  Rebase' (PtrListCons ptrA a) (PtrListCons ptrB b) c d e f g =>
  RebaseCont' False False ptrA ptrB a b c d e f g where
  rebaseCont' _ _ _ _ _ _ = rebase' (Proxy :: _ (PtrListCons ptrA a)) (Proxy :: _  (PtrListCons ptrB b))

instance rebaseSinOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (TSinOsc ptrA) NoEdge) iA (NodeC (TSinOsc ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = Frame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseHighpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (THighpass ptrA) eA) iA (NodeC (THighpass ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    Frame
      $ append <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB) <*> rest
    where
    Frame rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseGain ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (TGain ptrA) eA) iA (NodeC (TGain ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    Frame
      $ append <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB) <*>  rest
    where
    Frame rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseSpeaker ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (TSpeaker ptrA) eA) iA (NodeC (TSpeaker ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    Frame
      $ append <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB) <*> rest
    where
    Frame rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)
