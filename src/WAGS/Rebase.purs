module WAGS.Rebase
  ( class Rebase'
  , class RebaseCheck'
  , class RebaseCont'
  , rebase
  , reset
  , ResetSig
  , RebaseProof
  , rebase'
  , rebaseCheck'
  , rebaseCont'
  ) where

import Prelude
import Control.Monad.State (modify_)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set as S
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Bool (False, True)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (currentIdx, graph)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (AudioState, FrameT, unsafeFrame, unsafeUnframe)
import WAGS.Interpret (class AudioInterpret, rebaseAllUnits)
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinToInt, Ptr, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, Graph)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Validation (class LookupNL, class PtrInPtrList, class TerminalIdentityEdge)

data RebaseProof
  = RebaseProof

forceArray :: forall a. Array a -> Array a
forceArray = identity

-- | Rebase an Universe from `iA` to `iB`.
-- |
-- | Rebasing is used when you have an audio graph that goes through a series of mutations and winds
-- | up in the same state as before but with different pointers to audio nodes.
-- | For example, if you remove a node and add a node back in the same place with the same content,
-- | the universe will be semantically equivalent to the previous one but the type will be different
-- | because the pointers are different.  Rebasing takes the type from the output and "resets" it to
-- | the input.
-- |
-- | As an example, imagine that you have a function that mutates an audio graph
-- | through the following stages:
-- |
-- | - `TSpeaker D0 (TSinOsc D1 /\ TSinOsc D2 /\ Unit)`
-- | - `TSpeaker D0 (TSinOsc D2 /\ Unit)`
-- | - `TSpeaker D0 (TSinOsc D3 /\ TSinOsc D2 /\ Unit)`
-- |
-- | In this case, the output graph is topologically similar to the input graph, but a function that
-- | accepts the input type of `TSpeaker D0 (TSinOsc D1 /\ TSinOsc D2 /\ Unit)` would not accept the
-- | input type of `TSpeaker D0 (TSinOsc D3 /\ TSinOsc D2 /\ Unit)` because `D3` is not the same type as
-- | `D1`. In this instance, `rebase` will reset the output type back to the input type. If the rebase
-- | operation is attempted on graphs that are not topologically equivalent, a compile-time error will
-- | occur.
-- |
-- | Rebasing is useful if you want to call a function recursively. In that case, the input type needs
-- | to be the same each time the function is called, and rebase brings an audio graph back to the
-- | original type when possible.
-- |
-- | Rebasing only makes sense for functions that need to accommodate different loops.
-- | In the case of simpler loops, a similar effect can be achieved by making pointers polymorphic,
-- | ie `forall ptr. TSpeaker D0 (TSinOsc ptr /\ TSinOsc D2 /\ Unit)`.
rebase ::
  forall env audio engine proof m res changeBit skolems edgeA idxA graphA edgeB idxB graphB.
  BinToInt idxA =>
  BinToInt idxB =>
  Monad m =>
  AudioInterpret audio engine =>
  TerminalIdentityEdge graphA edgeA =>
  TerminalIdentityEdge graphB edgeB =>
  Rebase' PtrListNil PtrListNil RebaseProof edgeA idxA graphA edgeB idxB graphB =>
  Proxy idxA -> Proxy graphA -> Proxy idxB -> Proxy graphB -> FrameT env audio engine proof m res (UniverseC idxA graphA changeBit skolems) (UniverseC idxB graphB changeBit skolems) Unit
rebase _ _ _ _ =
  unsafeFrame do
    a <- (unsafeUnframe $ rebase' (Proxy :: _ PtrListNil) (Proxy :: _ PtrListNil) RebaseProof (Proxy :: _ edgeA) (Proxy :: _ idxA) (Proxy :: _ graphA) (Proxy :: _ edgeB) (Proxy :: _ idxB) (Proxy :: _ graphB))
    modify_ \i ->
      let
        mapping = M.fromFoldable $ map (\{ from, to } -> from /\ to) a
      in
        i
          { currentIdx = toInt' (Proxy :: _ idxB)
          , internalNodes =
            M.fromFoldable
              $ map
                  ( \(idx /\ v) ->
                      (fromMaybe idx $ M.lookup idx mapping) /\ v
                  )
              $ forceArray
              $ M.toUnfoldable
              $ i.internalNodes
          , internalEdges =
            M.fromFoldable
              $ map
                  ( \(idx /\ v) ->
                      (fromMaybe idx $ M.lookup idx mapping)
                        /\ (S.map (\idx' -> fromMaybe idx' $ M.lookup idx' mapping) v)
                  )
              $ forceArray
              $ M.toUnfoldable
              $ i.internalEdges
          , instructions = i.instructions <> [ rebaseAllUnits a ]
          }

-- | Signature for the reset operation for index `i1` and graph `g1`.
type ResetSig i1 g1
  = forall env audio engine proof m res cb e0 i0 g0 e1.
    BinToInt i0 =>
    BinToInt i1 =>
    Monad m =>
    AudioInterpret audio engine =>
    TerminalIdentityEdge g0 e0 =>
    TerminalIdentityEdge g1 e1 =>
    Rebase' PtrListNil PtrListNil RebaseProof e0 i0 g0 e1 i1 g1 =>
    FrameT env audio engine proof m res (UniverseC i0 g0 cb SkolemListNil) (UniverseC i1 g1 cb SkolemListNil) Unit

-- | Rebase the current audio graph to index `i1` and graph `g1`.
reset :: forall i1 g1. Proxy i1 -> Proxy g1 -> ResetSig i1 g1
reset i1 g1 = WAGS.do
  ci <- currentIdx
  g <- graph
  rebase ci g i1 g1

type AFT
  = (Array { from :: Int, to :: Int })

-- | An internal helper class used to help in the rebasing calculation.
class Rebase' :: forall k1 k2 k3 k4. k1 -> k2 -> Type -> k3 -> Ptr -> Graph -> k4 -> Ptr -> Graph -> Constraint
class Rebase' plA plB rbp pA ptrA graphA pB ptrB graphB where
  rebase' :: forall env audio engine proof m res changeBit skolems. Monad m => AudioInterpret audio engine => Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy ptrA -> Proxy graphA -> Proxy pB -> Proxy ptrB -> Proxy graphB -> FrameT env audio engine proof m res (UniverseC ptrA graphA changeBit skolems) (UniverseC ptrB graphB changeBit skolems) AFT

-- | An internal helper class used to help in the rebasing calculation.
class RebaseCheck' :: forall k1 k2 k3 k4. k1 -> k2 -> Type -> k3 -> Ptr -> Graph -> k4 -> Ptr -> Graph -> Constraint
class RebaseCheck' plA plB rbp pA ptrA graphA pB ptrB graphB where
  rebaseCheck' :: forall env audio engine proof m res changeBit skolems. Monad m => AudioInterpret audio engine => Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy ptrA -> Proxy graphA -> Proxy pB -> Proxy ptrB -> Proxy graphB -> FrameT env audio engine proof m res (UniverseC ptrA graphA changeBit skolems) (UniverseC ptrB graphB changeBit skolems) AFT

-- | An internal helper class used to help in the rebasing calculation.
class RebaseCont' :: forall k1 k2 k3 k4 k5 k6 k7 k8. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> Type -> k7 -> Ptr -> Graph -> k8 -> Ptr -> Graph -> Constraint
class RebaseCont' foundA foundB xpA xpB plA plB rbp pA ptrA graphA pB ptrB graphB where
  rebaseCont' :: forall env audio engine proof m res changeBit skolems. Monad m => AudioInterpret audio engine => Proxy foundA -> Proxy foundB -> Proxy xpA -> Proxy xpB -> Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy ptrA -> Proxy graphA -> Proxy pB -> Proxy ptrB -> Proxy graphB -> FrameT env audio engine proof m res (UniverseC ptrA graphA changeBit skolems) (UniverseC ptrB graphB changeBit skolems) AFT

rebaseAudioUnit ::
  forall env proof ptrA ptrB audio engine m res.
  Monad m =>
  BinToInt ptrA =>
  BinToInt ptrB =>
  AudioInterpret audio engine =>
  Proxy ptrA ->
  Proxy ptrB ->
  AudioState env audio engine proof m res AFT
rebaseAudioUnit ptrA ptrB = (pure <<< pure) { from: iA, to: iB }
  where
  iA = toInt' ptrA

  iB = toInt' ptrB

instance rebaseNoEdge :: Rebase' rblA rblB RebaseProof NoEdge ptrA graphA NoEdge ptrB graphB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame $ pure mempty

instance rebaseMany2 ::
  ( Rebase' rblA rblB RebaseProof (SingleEdge pA) ptrA graphA (SingleEdge pB) ptrB graphB
  , Rebase' rblA rblB RebaseProof (ManyEdges aA bA) ptrA graphA (ManyEdges aB bB) ptrB graphB
  ) =>
  Rebase' rblA rblB RebaseProof (ManyEdges pA (PtrListCons aA bA)) ptrA graphA (ManyEdges pB (PtrListCons aB bB)) ptrB graphB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (append <$> l <*> r)
    where
    l = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (SingleEdge pA)) (Proxy :: _ ptrA) (Proxy :: _ graphA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ ptrB) (Proxy :: _ graphB)

    r = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (ManyEdges aA bA)) (Proxy :: _ ptrA) (Proxy :: _ graphA) (Proxy :: _ (ManyEdges aB bB)) (Proxy :: _ ptrB) (Proxy :: _ graphB)

instance rebaseMany1 ::
  ( Rebase' rblA rblB RebaseProof (SingleEdge pA) ptrA gA (SingleEdge pB) ptrB gB
    ) =>
  Rebase' rblA rblB RebaseProof (ManyEdges pA PtrListNil) ptrA gA (ManyEdges pB PtrListNil) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (SingleEdge pA)) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseSingleEdge ::
  RebaseCheck' rblA rblB RebaseProof (SingleEdge pA) ptrA graphA (SingleEdge pB) ptrB graphB =>
  Rebase' rblA rblB RebaseProof (SingleEdge pA) ptrA graphA (SingleEdge pB) ptrB graphB where
  rebase' = rebaseCheck'

instance rebaseCheckSingleEdge ::
  ( GraphToNodeList gA nlA
  , LookupNL NodeListNil pA nlA (NodeListCons nodeA NodeListNil)
  , GraphToNodeList gB nlB
  , LookupNL NodeListNil pB nlB (NodeListCons nodeB NodeListNil)
  , PtrInPtrList False pA rblA tfA
  , PtrInPtrList False pB rblB tfB
  , RebaseCont' tfA tfB pA pB rblA rblB RebaseProof nodeA ptrA gA nodeB ptrB gB
  ) =>
  RebaseCheck' rblA rblB RebaseProof (SingleEdge pA) ptrA gA (SingleEdge pB) ptrB gB where
  rebaseCheck' _ _ _ _ _ _ _ _ _ = rebaseCont' (Proxy :: _ tfA) (Proxy :: _ tfB) (Proxy :: _ pA) (Proxy :: _ pB) (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ nodeA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ nodeB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseContTT ::
  RebaseCont' True True ptrA ptrB rblA rblB RebaseProof a b c d e f where
  rebaseCont' _ _ _ _ _ _ _ _ _ _ _ _ _ = unsafeFrame $ pure mempty

instance rebaseContFF ::
  Rebase' (PtrListCons ptrA a) (PtrListCons ptrB b) RebaseProof d e f g h i =>
  RebaseCont' False False ptrA ptrB a b RebaseProof d e f g h i where
  rebaseCont' _ _ _ _ _ _ = rebase' (Proxy :: _ (PtrListCons ptrA a)) (Proxy :: _ (PtrListCons ptrB b))

--------------------------------------
instance rebaseAllpass ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TAllpass pA) eA) ptrA gA (NodeC (AU.TAllpass pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseBandpass ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TBandpass pA) eA) ptrA gA (NodeC (AU.TBandpass pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseConstant ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TConstant pA) NoEdge) ptrA gA (NodeC (AU.TConstant pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebaseConvolver ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TConvolver pA name) eA) ptrA gA (NodeC (AU.TConvolver pB name) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseDelay ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TDelay pA) eA) ptrA gA (NodeC (AU.TDelay pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseDynamicsCompressor ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TDynamicsCompressor pA) eA) ptrA gA (NodeC (AU.TDynamicsCompressor pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseGain ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TGain pA) eA) ptrA gA (NodeC (AU.TGain pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseHighpass ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.THighpass pA) eA) ptrA gA (NodeC (AU.THighpass pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseHighshelf ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.THighshelf pA) eA) ptrA gA (NodeC (AU.THighshelf pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseLoopBuf ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLoopBuf pA) NoEdge) ptrA gA (NodeC (AU.TLoopBuf pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebaseLowpass ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLowpass pA) eA) ptrA gA (NodeC (AU.TLowpass pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseLowshelf ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLowshelf pA) eA) ptrA gA (NodeC (AU.TLowshelf pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseMicrophone ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TMicrophone pA) NoEdge) ptrA gA (NodeC (AU.TMicrophone pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebaseNotch ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TNotch pA) eA) ptrA gA (NodeC (AU.TNotch pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebasePeaking ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPeaking pA) eA) ptrA gA (NodeC (AU.TPeaking pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebasePeriodicOsc ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPeriodicOsc pA) NoEdge) ptrA gA (NodeC (AU.TPeriodicOsc pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebasePlayBuf ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPlayBuf pA) NoEdge) ptrA gA (NodeC (AU.TPlayBuf pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebaseRecorder ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TRecorder pA name) eA) ptrA gA (NodeC (AU.TRecorder pB name) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseSawtoothOsc ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSawtoothOsc pA) NoEdge) ptrA gA (NodeC (AU.TSawtoothOsc pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebaseSinOsc ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSinOsc pA) NoEdge) ptrA gA (NodeC (AU.TSinOsc pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebaseSpeaker ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSpeaker pA) eA) ptrA gA (NodeC (AU.TSpeaker pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseSquareOsc ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSquareOsc pA) NoEdge) ptrA gA (NodeC (AU.TSquareOsc pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebaseStereoPanner ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TStereoPanner pA) eA) ptrA gA (NodeC (AU.TStereoPanner pB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseTriangleOsc ::
  (BinToInt pA, BinToInt pB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TTriangleOsc pA) NoEdge) ptrA gA (NodeC (AU.TTriangleOsc pB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB))

instance rebaseWaveShaper ::
  (BinToInt pA, BinToInt pB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TWaveShaper pA name) eA) ptrA gA (NodeC (AU.TWaveShaper pB name) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ pA) (Proxy :: _ pB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

--------------------------------------
