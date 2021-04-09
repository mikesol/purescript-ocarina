module WAGS.Rebase where

import Prelude

import Control.Monad.State (modify_)
import Data.Typelevel.Bool (False, True)
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (FrameT(..), AudioState)
import WAGS.Interpret (class AudioInterpret, rebaseAllUnits)
import WAGS.Universe.AudioUnit as AU
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
  rebase :: forall env audio engine proof m. Monad m => AudioInterpret audio engine => Proxy iA -> Proxy iB -> FrameT env audio engine proof m iA iB Unit

instance rebaseRebase ::
  ( TerminalIdentityEdge graphA edgeA
  , TerminalIdentityEdge graphB edgeB
  , Rebase' PtrListNil PtrListNil RebaseProof edgeA (idxA graphA changeBitA skolemsA) edgeB (idxB graphB changeBitB skolemsB)
  ) =>
  Rebase (idxA graphA changeBitA skolemsA) (idxB graphB changeBitB skolemsB) where
  rebase iA iB =
    FrameT do
      a <- arr
      modify_ \i ->
        i
          { instructions = i.instructions <> [ rebaseAllUnits a ]
          }
    where
    FrameT arr = rebase' (Proxy :: _ PtrListNil) (Proxy :: _ PtrListNil) RebaseProof (Proxy :: _ edgeA) iA (Proxy :: _ edgeB) iB

type AFT
  = (Array { from :: Int, to :: Int })

-- private, only used to collect rebase instructions
class Rebase' :: forall k1. PtrList -> PtrList -> Type -> k1 -> Universe -> k1 -> Universe -> Constraint
class Rebase' plA plB rbp pA (iA :: Universe) pB (iB :: Universe) where
  rebase' :: forall env audio engine proof m. Monad m => AudioInterpret audio engine => Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy iA -> Proxy pB -> Proxy iB -> FrameT env audio engine proof m iA iB AFT

class RebaseCheck' :: forall k1. PtrList -> PtrList -> Type -> k1 -> Universe -> k1 -> Universe -> Constraint
class RebaseCheck' plA plB rbp pA (iA :: Universe) pB (iB :: Universe) where
  rebaseCheck' :: forall env audio engine proof m. Monad m => AudioInterpret audio engine => Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy iA -> Proxy pB -> Proxy iB -> FrameT env audio engine proof m iA iB AFT

class RebaseCont' :: forall k1. Type -> Type -> Ptr -> Ptr -> PtrList -> PtrList -> Type -> k1 -> Universe -> k1 -> Universe -> Constraint
class RebaseCont' foundA foundB ptrA ptrB plA plB rbp pA (iA :: Universe) pB (iB :: Universe) where
  rebaseCont' :: forall env audio engine proof m. Monad m => AudioInterpret audio engine => Proxy foundA -> Proxy foundB -> Proxy ptrA -> Proxy ptrB -> Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy iA -> Proxy pB -> Proxy iB -> FrameT env audio engine proof m iA iB AFT

rebaseAudioUnit ::
  forall env proof ptrA ptrB audio engine m.
  Monad m =>
  BinToInt ptrA =>
  BinToInt ptrB =>
  AudioInterpret audio engine =>
  Proxy ptrA ->
  Proxy ptrB ->
  AudioState env audio engine proof m AFT
rebaseAudioUnit ptrA ptrB = if iA == iB then pure mempty else (pure <<< pure) { from: iA, to: iB }
  where
  iA = toInt' ptrA

  iB = toInt' ptrB

instance rebaseNoEdge :: Rebase' rblA rblB RebaseProof NoEdge iA NoEdge iB where
  rebase' _ _ _ _ _ _ _ = FrameT $ pure mempty

instance rebaseMany2 ::
  ( Rebase' rblA rblB RebaseProof (SingleEdge pA) iA (SingleEdge pB) iB
  , Rebase' rblA rblB RebaseProof (ManyEdges aA bA) iA (ManyEdges aB bB) iB
  ) =>
  Rebase' rblA rblB RebaseProof (ManyEdges pA (PtrListCons aA bA)) iA (ManyEdges pB (PtrListCons aB bB)) iB where
  rebase' _ _ _ _ iA _ iB = FrameT (append <$> l <*> r)
    where
    FrameT l = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (SingleEdge pA)) (Proxy :: _ iA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ iB)

    FrameT r = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (ManyEdges aA bA)) (Proxy :: _ iA) (Proxy :: _ (ManyEdges aB bB)) (Proxy :: _ iB)

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
  rebaseCont' _ _ _ _ _ _ _ _ _ _ _ = FrameT $ pure mempty

instance rebaseContFF ::
  Rebase' (PtrListCons ptrA a) (PtrListCons ptrB b) c d e f g =>
  RebaseCont' False False ptrA ptrB a b c d e f g where
  rebaseCont' _ _ _ _ _ _ = rebase' (Proxy :: _ (PtrListCons ptrA a)) (Proxy :: _ (PtrListCons ptrB b))

--------------------------------------
instance rebaseAllpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TAllpass ptrA) eA) iA (NodeC (AU.TAllpass ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseBandpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TBandpass ptrA) eA) iA (NodeC (AU.TBandpass ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseConstant ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TConstant ptrA) NoEdge) iA (NodeC (AU.TConstant ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseConvolver ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TConvolver ptrA) eA) iA (NodeC (AU.TConvolver ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseDelay ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TDelay ptrA) eA) iA (NodeC (AU.TDelay ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseDynamicsCompressor ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TDynamicsCompressor ptrA) eA) iA (NodeC (AU.TDynamicsCompressor ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseGain ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TGain ptrA) eA) iA (NodeC (AU.TGain ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseHighpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.THighpass ptrA) eA) iA (NodeC (AU.THighpass ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseHighshelf ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.THighshelf ptrA) eA) iA (NodeC (AU.THighshelf ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseLoopBuf ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLoopBuf ptrA) NoEdge) iA (NodeC (AU.TLoopBuf ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseLowpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLowpass ptrA) eA) iA (NodeC (AU.TLowpass ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseLowshelf ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLowshelf ptrA) eA) iA (NodeC (AU.TLowshelf ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseMicrophone ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TMicrophone ptrA) NoEdge) iA (NodeC (AU.TMicrophone ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseNotch ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TNotch ptrA) eA) iA (NodeC (AU.TNotch ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebasePeaking ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPeaking ptrA) eA) iA (NodeC (AU.TPeaking ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebasePeriodicOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPeriodicOsc ptrA) NoEdge) iA (NodeC (AU.TPeriodicOsc ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebasePlayBuf ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPlayBuf ptrA) NoEdge) iA (NodeC (AU.TPlayBuf ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseRecorder ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TRecorder ptrA) eA) iA (NodeC (AU.TRecorder ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseSawtoothOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSawtoothOsc ptrA) NoEdge) iA (NodeC (AU.TSawtoothOsc ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseSinOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSinOsc ptrA) NoEdge) iA (NodeC (AU.TSinOsc ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseSpeaker ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSpeaker ptrA) eA) iA (NodeC (AU.TSpeaker ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseSquareOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSquareOsc ptrA) NoEdge) iA (NodeC (AU.TSquareOsc ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseStereoPanner ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TStereoPanner ptrA) eA) iA (NodeC (AU.TStereoPanner ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

instance rebaseTriangleOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TTriangleOsc ptrA) NoEdge) iA (NodeC (AU.TTriangleOsc ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = FrameT (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseWaveShaper ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TWaveShaper ptrA) eA) iA (NodeC (AU.TWaveShaper ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    FrameT
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    FrameT rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)

--------------------------------------
