module WAGS.Cursor where

import Prelude

import Data.Identity (Identity)
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Create (class Create)
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Decorators (Focus, This, IgnoreMe)
import WAGS.Universe.AudioUnit (AudioUnitRef(..))
import WAGS.Universe.AudioUnit as AU
import WAGS.Universe.Bin (class BinSub, class BinToInt, Ptr, PtrList, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.BinN (D0)
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, Graph, InitialGraph)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class ToSkolemizedFunction, SkolemListCons, SkolemPairC)
import WAGS.Universe.Universe (UniverseC)
import WAGS.Validation (class AltEdgeProfile, class PtrListAppend, class TerminalIdentityEdge)

-- | Focus on a particular audio unit in a graph. Use in conjunction with `change` to modify
-- | an audio unit.
-- |
-- | ```purescript
-- | myCursor <- cursor (Speaker (Gain 1.0 (SinOsc 440.0 /\ Focus (SinOsc 330.0) /\ Unit)))
-- | changeAt myCursor (SinOsc 332.0)
-- | ```
-- |
-- | Graphs can be focused on different elements, allowing multiple cursors to be generated from
-- | the same graph.
-- | 
-- | ```purescript
-- | let
-- |   graph a b = Speaker (Gain 1.0 (a (SinOsc 440.0) /\ b (SinOsc 330.0) /\ Unit))
-- | cursor1 <- cursor (graph Focus Identity)
-- | cursor2 <- cursor (graph Identity Focus)
-- | changeAt cursor1 (SinOsc 444.0)
-- | changeAt cursor2 (SinOsc 332.0)
-- | ```
-- |
-- | For more complex graphs, decorators can be used to auto-generate cursors.
-- | See `WAGS.Graph.Decorators`.
-- |
-- | To see examples of cursors, check out `test/Ops.purs` and `examples/wtk/WTK/TLP.purs`.
cursor ::
  forall edge audio engine a q r s t env proof m res p.
  Monad m =>
  TerminalIdentityEdge r edge =>
  Cursor edge a r p =>
  BinToInt p =>
  a -> FrameT env audio engine proof m res (UniverseC q r s t) (UniverseC q r s t) (AudioUnitRef p)
cursor = cursor' (Proxy :: _ edge)

-- | Like `cursor`, but starting from an arbitrary edge in a graph. This is useful when, for example,
-- | the cursor needs to be obtained for an audio unit that is not yet connected to a speaker.  In
-- | most cases, however, you'll want to use `cursor`, which uses `Speaker` as the top-most unit.
class Cursor (p :: EdgeProfile) (a :: Type) (g :: Graph) (ptr :: Ptr) | p a g -> ptr where
  cursor' :: forall env audio engine proof m res currentIdx changeBit skolems. Monad m => Proxy p -> a -> FrameT env audio engine proof m res (UniverseC currentIdx g changeBit skolems) (UniverseC currentIdx g changeBit skolems) (AudioUnitRef ptr)

instance cursorRecurse ::
  ( BinToInt head
  , CursorI p a o (PtrListCons head PtrListNil)
  ) =>
  Cursor p a o head where
  cursor' _ _ = unsafeFrame $ (pure $ AudioUnitRef (toInt' (Proxy :: Proxy head)))

class CursorRes (tag :: Type) (p :: Ptr) (i :: Node) (plist :: EdgeProfile) | tag p i -> plist

instance cursorResAllpass :: CursorRes (CTOR.Allpass a b c) ptr (NodeC (AU.TAllpass ptr) edge) edge
else instance cursorResBandpass :: CursorRes (CTOR.Bandpass a b c) ptr (NodeC (AU.TBandpass ptr) edge) edge
else instance cursorResConstant :: CursorRes (CTOR.Constant a) ptr (NodeC (AU.TConstant ptr) edge) edge
else instance cursorResConvolver :: CursorRes (CTOR.Convolver name b) ptr (NodeC (AU.TConvolver ptr name) edge) edge
else instance cursorResDelay :: CursorRes (CTOR.Delay a b) ptr (NodeC (AU.TDelay ptr) edge) edge
else instance cursorResDynamicsCompressor :: CursorRes (CTOR.DynamicsCompressor a b c d e f) ptr (NodeC (AU.TDynamicsCompressor ptr) edge) edge
else instance cursorResGain :: CursorRes (CTOR.Gain a b) ptr (NodeC (AU.TGain ptr) edge) edge
else instance cursorResHighpass :: CursorRes (CTOR.Highpass a b c) ptr (NodeC (AU.THighpass ptr) edge) edge
else instance cursorResHighshelf :: CursorRes (CTOR.Highshelf a b c) ptr (NodeC (AU.THighshelf ptr) edge) edge
else instance cursorResLoopBuf :: CursorRes (CTOR.LoopBuf b) ptr (NodeC (AU.TLoopBuf ptr) edge) edge
else instance cursorResLowpass :: CursorRes (CTOR.Lowpass a b c) ptr (NodeC (AU.TLowpass ptr) edge) edge
else instance cursorResLowshelf :: CursorRes (CTOR.Lowshelf a b c) ptr (NodeC (AU.TLowshelf ptr) edge) edge
else instance cursorResMicrophone :: CursorRes (CTOR.Microphone) ptr (NodeC (AU.TMicrophone ptr) edge) edge
else instance cursorResNotch :: CursorRes (CTOR.Notch a b c) ptr (NodeC (AU.TNotch ptr) edge) edge
else instance cursorResPeaking :: CursorRes (CTOR.Peaking a b c d) ptr (NodeC (AU.TPeaking ptr) edge) edge
else instance cursorResPeriodicOsc :: CursorRes (CTOR.PeriodicOsc b) ptr (NodeC (AU.TPeriodicOsc ptr) edge) edge
else instance cursorResPlayBuf :: CursorRes (CTOR.PlayBuf b) ptr (NodeC (AU.TPlayBuf ptr) edge) edge
else instance cursorResRecorder :: CursorRes (CTOR.Recorder name b) ptr (NodeC (AU.TRecorder ptr name) edge) edge
else instance cursorResSawtoothOsc :: CursorRes (CTOR.SawtoothOsc a) ptr (NodeC (AU.TSawtoothOsc ptr) edge) edge
else instance cursorResSinOsc :: CursorRes (CTOR.SinOsc a) ptr (NodeC (AU.TSinOsc ptr) edge) edge
else instance cursorResSpeaker :: CursorRes (CTOR.Speaker a) ptr (NodeC (AU.TSpeaker ptr) edge) edge
else instance cursorResSquareOsc :: CursorRes (CTOR.SquareOsc a) ptr (NodeC (AU.TSquareOsc ptr) edge) edge
else instance cursorResStereoPanner :: CursorRes (CTOR.StereoPanner a b) ptr (NodeC (AU.TStereoPanner ptr) edge) edge
else instance cursorResTriangleOsc :: CursorRes (CTOR.TriangleOsc a) ptr (NodeC (AU.TTriangleOsc ptr) edge) edge
else instance cursorResWaveShaper :: CursorRes (CTOR.WaveShaper name b c) ptr (NodeC (AU.TWaveShaper ptr name) edge) edge
else instance cursorResMiss :: CursorRes tag p n NoEdge

-- | Internal helper class used for Cursor.
class Cursor' (tag :: Type) (p :: Ptr) (i :: NodeList) (nextP :: EdgeProfile) | tag p i -> nextP

instance cursorNil :: Cursor' tag p NodeListNil NoEdge

instance cursorCons ::
  ( CursorRes tag p head headPlist
  , Cursor' tag p tail tailPlist
  , AltEdgeProfile headPlist tailPlist plist
  ) =>
  Cursor' tag p (NodeListCons head tail) plist

-- | Internal helper class used for Cursor.
class CursorX (tag :: Type) (p :: Ptr) (i :: Graph) (nextP :: EdgeProfile) | tag p i -> nextP

instance cursorX ::
  ( GraphToNodeList ig il
  , Cursor' tag p il nextP
  ) =>
  CursorX tag p ig nextP

-- | Internal helper class used for Cursor.
class CursorI (p :: EdgeProfile) (a :: Type) (g :: Graph) (ptr :: PtrList) | p a g -> ptr

instance cursorNoEdge :: CursorI NoEdge g igraph PtrListNil

instance cursorSkolem :: BinToInt p => CursorI (SingleEdge p) (Proxy skolem) igraph PtrListNil

instance cursorIdentity :: (BinToInt p, CursorI (SingleEdge p) x igraph o) => CursorI (SingleEdge p) (Identity x) igraph o

instance cursorFocus :: (BinToInt p, CursorI (SingleEdge p) x igraph o) => CursorI (SingleEdge p) (Focus x) igraph (PtrListCons p o)

instance cursorThis :: CursorI (SingleEdge p) This igraph (PtrListCons p PtrListNil)

instance cursorIgnoreMe :: CursorI (SingleEdge p) IgnoreMe igraph PtrListNil

instance cursorMany2 ::
  ( BinToInt p
  , BinToInt a
  , CursorI (SingleEdge p) x igraph o0
  , CursorI (ManyEdges a b) y igraph o1
  , PtrListAppend o0 o1 oo
  ) =>
  CursorI (ManyEdges p (PtrListCons a b)) (x /\ y) igraph oo

instance cursorMany1 ::
  (BinToInt p, CursorI (SingleEdge p) a igraph o) =>
  CursorI (ManyEdges p PtrListNil) (a /\ Unit) igraph o

-- incoming to the change will be the ptr of the inner closure, which is the actual connection -- we run the inner closure to get the ptr for the outer closure
instance cursorDup ::
  ( Create
      a
      D0
      InitialGraph
      (SkolemListCons (SkolemPairC skolem D0) skolems)
      outptr
      grapho
      (SkolemListCons (SkolemPairC skolem D0) skolems)
      ignore
  , BinToInt p
  , BinToInt outptr
  , BinToInt continuation
  , BinSub p outptr continuation
  , CursorI (SingleEdge p) b igraph o0
  , CursorI (SingleEdge continuation) a igraph o1
  , PtrListAppend o0 o1 oo
  ) =>
  CursorI (SingleEdge p) (CTOR.Dup a (Proxy skolem -> b)) igraph oo

--------------
instance cursorAllpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Allpass argA argB argC) p igraph nextP
  , CursorI nextP argC igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Allpass argA argB fOfargC) igraph o

instance cursorBandpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Bandpass argA argB argC) p igraph nextP
  , CursorI nextP argC igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Bandpass argA argB fOfargC) igraph o

instance cursorConstant ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.Constant argA) igraph PtrListNil

instance cursorConvolver ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , CursorX (CTOR.Convolver argA argB) p igraph nextP
  , CursorI nextP argB igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Convolver argA fOfargB) igraph o

instance cursorDelay ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , CursorX (CTOR.Delay argA argB) p igraph nextP
  , CursorI nextP argB igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Delay argA fOfargB) igraph o

instance cursorDynamicsCompressor ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargF skolem
  , ToSkolemizedFunction fOfargF skolem argF
  , CursorX (CTOR.DynamicsCompressor argA argB argC argD argE argF) p igraph nextP
  , CursorI nextP argF igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.DynamicsCompressor argA argB argC argD argE fOfargF) igraph o

instance cursorHighpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Highpass argA argB argC) p igraph nextP
  , CursorI nextP argC igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Highpass argA argB fOfargC) igraph o

instance cursorHighshelf ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Highshelf argA argB argC) p igraph nextP
  , CursorI nextP argC igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Highshelf argA argB fOfargC) igraph o

instance cursorLoopBuf ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.LoopBuf argB) igraph PtrListNil

instance cursorLowpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Lowpass argA argB argC) p igraph nextP
  , CursorI nextP argC igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Lowpass argA argB fOfargC) igraph o

instance cursorLowshelf ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Lowshelf argA argB argC) p igraph nextP
  , CursorI nextP argC igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Lowshelf argA argB fOfargC) igraph o

instance cursorMicrophone ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.Microphone) igraph PtrListNil

instance cursorNotch ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Notch argA argB argC) p igraph nextP
  , CursorI nextP argC igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Notch argA argB fOfargC) igraph o

instance cursorPeaking ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargD skolem
  , ToSkolemizedFunction fOfargD skolem argD
  , CursorX (CTOR.Peaking argA argB argC argD) p igraph nextP
  , CursorI nextP argD igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Peaking argA argB argC fOfargD) igraph o

instance cursorPeriodicOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.PeriodicOsc argB) igraph PtrListNil

instance cursorPlayBuf ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.PlayBuf argB) igraph PtrListNil

instance cursorRecorder ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , CursorX (CTOR.Recorder argA argB) p igraph nextP
  , CursorI nextP argB igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Recorder argA fOfargB) igraph o

instance cursorSawtoothOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.SawtoothOsc argA) igraph PtrListNil

instance cursorSinOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.SinOsc argA) igraph PtrListNil

instance cursorSquareOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.SquareOsc argA) igraph PtrListNil

instance cursorStereoPanner ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , CursorX (CTOR.StereoPanner argA argB) p igraph nextP
  , CursorI nextP argB igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.StereoPanner argA fOfargB) igraph o

instance cursorTriangleOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.TriangleOsc argA) igraph PtrListNil

instance cursorWaveShaper ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.WaveShaper argA argB argC) p igraph nextP
  , CursorI nextP argC igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.WaveShaper argA argB fOfargC) igraph o

---------------
instance cursorGain ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , CursorX (CTOR.Gain a b) p igraph nextP
  , CursorI nextP b igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Gain a fb) igraph o

instance cursorSpeaker ::
  ( BinToInt p
  , CursorX (CTOR.Speaker a) p igraph nextP
  , CursorI nextP a igraph o
  ) =>
  CursorI (SingleEdge p) (CTOR.Speaker a) igraph o
