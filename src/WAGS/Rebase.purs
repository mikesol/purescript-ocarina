module WAGS.Rebase
  ( class Rebase'
  , class RebaseCheck'
  , class RebaseCont'
  , rebase
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
import WAGS.Control.Types (AudioState, FrameT, unsafeFrame, unsafeUnframe)
import WAGS.Interpret (class AudioInterpret, rebaseAllUnits)
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinToInt, PtrListCons, PtrListNil, Ptr, toInt')
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, Graph)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)
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
  forall env audio engine proof m changeBit skolems edgeA idxA graphA edgeB idxB graphB.
  Monad m =>
  AudioInterpret audio engine =>
  TerminalIdentityEdge graphA edgeA =>
  TerminalIdentityEdge graphB edgeB =>
  Rebase' PtrListNil PtrListNil RebaseProof edgeA idxA graphA edgeB idxB graphB =>
  Proxy idxA -> Proxy graphA -> Proxy idxB -> Proxy graphB -> FrameT env audio engine proof m (UniverseC idxA graphA changeBit skolems) (UniverseC idxB graphB changeBit skolems) Unit
rebase ptrA gA ptrB gB =
  unsafeFrame do
    a <- (unsafeUnframe $ rebase' (Proxy :: _ PtrListNil) (Proxy :: _ PtrListNil) RebaseProof (Proxy :: _ edgeA) (Proxy :: _ idxA) (Proxy :: _ graphA) (Proxy :: _ edgeB) (Proxy :: _ idxB) (Proxy :: _ graphB))
    modify_ \i ->
      let
        mapping = M.fromFoldable $ map (\{ from, to } -> from /\ to) a
      in
        i
          { internalNodes =
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

type AFT
  = (Array { from :: Int, to :: Int })

-- | An internal helper class used to help in the rebasing calculation.
class Rebase' :: forall k1 k2 k3 k4. k1 -> k2 -> Type -> k3 -> Ptr -> Graph -> k4 -> Ptr -> Graph -> Constraint
class Rebase' plA plB rbp pA ptrA graphA pB ptrB graphB where
  rebase' :: forall env audio engine proof m changeBit skolems. Monad m => AudioInterpret audio engine => Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy ptrA -> Proxy graphA -> Proxy pB -> Proxy ptrB -> Proxy graphB -> FrameT env audio engine proof m (UniverseC ptrA graphA changeBit skolems) (UniverseC ptrB graphB changeBit skolems) AFT

-- | An internal helper class used to help in the rebasing calculation.
class RebaseCheck' :: forall k1 k2 k3 k4. k1 -> k2 -> Type -> k3 -> Ptr -> Graph -> k4 -> Ptr -> Graph -> Constraint
class RebaseCheck' plA plB rbp pA ptrA graphA pB ptrB graphB where
  rebaseCheck' :: forall env audio engine proof m changeBit skolems. Monad m => AudioInterpret audio engine => Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy ptrA -> Proxy graphA -> Proxy pB -> Proxy ptrB -> Proxy graphB -> FrameT env audio engine proof m (UniverseC ptrA graphA changeBit skolems) (UniverseC ptrB graphB changeBit skolems) AFT

-- | An internal helper class used to help in the rebasing calculation.
class RebaseCont' :: forall k1 k2 k3 k4 k5 k6 k7 k8. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> Type -> k7 -> Ptr -> Graph -> k8 -> Ptr -> Graph -> Constraint
class RebaseCont' foundA foundB xpA xpB plA plB rbp pA ptrA graphA pB ptrB graphB where
  rebaseCont' :: forall env audio engine proof m changeBit skolems. Monad m => AudioInterpret audio engine => Proxy foundA -> Proxy foundB -> Proxy xpA -> Proxy xpB -> Proxy plA -> Proxy plB -> rbp -> Proxy pA -> Proxy ptrA -> Proxy graphA -> Proxy pB -> Proxy ptrB -> Proxy graphB -> FrameT env audio engine proof m (UniverseC ptrA graphA changeBit skolems) (UniverseC ptrB graphB changeBit skolems) AFT

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

instance rebaseNoEdge :: Rebase' rblA rblB RebaseProof NoEdge ptrA graphA NoEdge ptrB graphB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame $ pure mempty

instance rebaseMany2 ::
  ( Rebase' rblA rblB RebaseProof (SingleEdge pA) ptrA graphA (SingleEdge pB) ptrB graphB
  , Rebase' rblA rblB RebaseProof (ManyEdges aA bA) ptrA graphA (ManyEdges aB bB) ptrB graphB
  ) =>
  Rebase' rblA rblB RebaseProof (ManyEdges pA (PtrListCons aA bA)) ptrA graphA (ManyEdges pB (PtrListCons aB bB)) ptrB graphB where
  rebase' _ _ _ _ _ptrA graphA _ ptrB graphB = unsafeFrame (append <$> l <*> r)
    where
    l = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (SingleEdge pA)) (Proxy :: _ ptrA) (Proxy :: _ graphA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ ptrB) (Proxy :: _ graphB)

    r = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (ManyEdges aA bA)) (Proxy :: _ ptrA) (Proxy :: _ graphA) (Proxy :: _ (ManyEdges aB bB)) (Proxy :: _ ptrB) (Proxy :: _ graphB)

instance rebaseMany1 ::
  ( Rebase' rblA rblB RebaseProof (SingleEdge pA) ptrA gA (SingleEdge pB) ptrB gB
    ) =>
  Rebase' rblA rblB RebaseProof (ManyEdges pA PtrListNil) ptrA gA (ManyEdges pB PtrListNil) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ (SingleEdge pA)) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ (SingleEdge pB)) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseSingleEdge ::
  RebaseCheck' rblA rblB  RebaseProof (SingleEdge pA) ptrA graphA (SingleEdge pB) ptrB graphB =>
  Rebase' rblA rblB RebaseProof (SingleEdge pA) ptrA graphA (SingleEdge pB) ptrB graphB where
  rebase' = rebaseCheck'

instance rebaseCheckSingleEdge ::
  ( GraphToNodeList gA nlA
  , LookupNL NodeListNil pA nlA (NodeListCons (NodeC igA epA) NodeListNil)
  , GraphToNodeList gB nlB
  , LookupNL NodeListNil pB nlB (NodeListCons (NodeC igB epB) NodeListNil)
  , PtrInPtrList False pA rblA tfA
  , PtrInPtrList False pB rblB tfB
  , RebaseCont' tfA tfB pA pB rblA rblB RebaseProof epA ptrA gA epB ptrB gB
  ) =>
  RebaseCheck' rblA rblB RebaseProof (SingleEdge pA) ptrA gA (SingleEdge pB) ptrB gB where
  rebaseCheck' _ _ _ _ ptrA gA _ ptrB gB = rebaseCont' (Proxy :: _ tfA) (Proxy :: _ tfB) (Proxy :: _ pA) (Proxy :: _ pB) (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ epA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ epB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseContTT ::
  RebaseCont' True True ptrA ptrB rblA rblB RebaseProof a b c d e f where
  rebaseCont' _ _ _ _ _ _ _ _ _ _ _ _ _ = unsafeFrame $ pure mempty

instance rebaseContFF ::
  Rebase' (PtrListCons ptrA a) (PtrListCons ptrB b) c d e f g h i =>
  RebaseCont' False False ptrA ptrB a b c d e f g h i where
  rebaseCont' _ _ _ _ _ _ = rebase' (Proxy :: _ (PtrListCons ptrA a)) (Proxy :: _ (PtrListCons ptrB b))

--------------------------------------
instance rebaseAllpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TAllpass ptrA) eA) ptrA gA (NodeC (AU.TAllpass ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseBandpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TBandpass ptrA) eA) ptrA gA (NodeC (AU.TBandpass ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseConstant ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TConstant ptrA) NoEdge) ptrA gA (NodeC (AU.TConstant ptrB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseConvolver ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TConvolver ptrA name) eA) ptrA gA (NodeC (AU.TConvolver ptrB name) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseDelay ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TDelay ptrA) eA) ptrA gA (NodeC (AU.TDelay ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseDynamicsCompressor ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TDynamicsCompressor ptrA) eA) ptrA gA (NodeC (AU.TDynamicsCompressor ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseGain ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TGain ptrA) eA) ptrA gA (NodeC (AU.TGain ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseHighpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.THighpass ptrA) eA) ptrA gA (NodeC (AU.THighpass ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseHighshelf ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.THighshelf ptrA) eA) ptrA gA (NodeC (AU.THighshelf ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseLoopBuf ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLoopBuf ptrA name) NoEdge) ptrA gA (NodeC (AU.TLoopBuf ptrB name) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseLowpass ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLowpass ptrA) eA) ptrA gA (NodeC (AU.TLowpass ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseLowshelf ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TLowshelf ptrA) eA) ptrA gA (NodeC (AU.TLowshelf ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseMicrophone ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TMicrophone ptrA) NoEdge) ptrA gA (NodeC (AU.TMicrophone ptrB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseNotch ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TNotch ptrA) eA) ptrA gA (NodeC (AU.TNotch ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebasePeaking ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPeaking ptrA) eA) ptrA gA (NodeC (AU.TPeaking ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebasePeriodicOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPeriodicOsc ptrA name) NoEdge) ptrA gA (NodeC (AU.TPeriodicOsc ptrB name) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebasePlayBuf ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TPlayBuf ptrA name) NoEdge) ptrA gA (NodeC (AU.TPlayBuf ptrB name) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseRecorder ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TRecorder ptrA name) eA) ptrA gA (NodeC (AU.TRecorder ptrB name) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseSawtoothOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSawtoothOsc ptrA) NoEdge) ptrA gA (NodeC (AU.TSawtoothOsc ptrB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseSinOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSinOsc ptrA) NoEdge) ptrA gA (NodeC (AU.TSinOsc ptrB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseSpeaker ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSpeaker ptrA) eA) ptrA gA (NodeC (AU.TSpeaker ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseSquareOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TSquareOsc ptrA) NoEdge) ptrA gA (NodeC (AU.TSquareOsc ptrB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseStereoPanner ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TStereoPanner ptrA) eA) ptrA gA (NodeC (AU.TStereoPanner ptrB) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

instance rebaseTriangleOsc ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TTriangleOsc ptrA) NoEdge) ptrA gA (NodeC (AU.TTriangleOsc ptrB) NoEdge) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ = unsafeFrame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))

instance rebaseWaveShaper ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA ptrA gA eB ptrB gB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.TWaveShaper ptrA name) eA) ptrA gA (NodeC (AU.TWaveShaper ptrB name) eB) ptrB gB where
  rebase' _ _ _ _ _ _ _ _ _ =
    unsafeFrame
      $ append
      <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB)
      <*> rest
    where
    rest = unsafeUnframe $ rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ ptrA) (Proxy :: _ gA) (Proxy :: _ eB) (Proxy :: _ ptrB) (Proxy :: _ gB)

--------------------------------------
