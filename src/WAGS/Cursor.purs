module WAGS.Cursor where

import Prelude
import Data.Identity (Identity)
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import WAGS.Graph.Constructors as CTOR
import WAGS.Control.Types (FrameT(..))
import WAGS.Create (class Create)
--import WAGS.Graph.Constructors (Dup, Gain, Highpass, SinOsc, Speaker)
import WAGS.Universe.AudioUnit as AU
import WAGS.Graph.Decorators (Focus)
import WAGS.Universe.AudioUnit (AudioUnitRef(..))
import WAGS.Universe.Bin (class BinSub, class BinToInt, D0, Ptr, PtrList, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, InitialGraph)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class ToSkolemizedFunction, SkolemListCons, SkolemPairC)
import WAGS.Universe.Universe (Universe, UniverseC)
import WAGS.Validation (class EdgeProfileChooseGreater, class PtrListAppend, class TerminalIdentityEdge)

cursor ::
  forall edge a q r s t env proof m p.
  Monad m =>
  TerminalIdentityEdge r edge =>
  Cursor edge a (UniverseC q r s t) p =>
  BinToInt p =>
  a -> FrameT env proof m (UniverseC q r s t) (UniverseC q r s t) (AudioUnitRef p)
cursor = cursor' (Proxy :: _ edge)

class Cursor (p :: EdgeProfile) (a :: Type) (o :: Universe) (ptr :: Ptr) | p a o -> ptr where
  cursor' :: forall env proof m. Monad m => Proxy p -> a -> FrameT env proof m o o (AudioUnitRef ptr)

instance cursorRecurse ::
  ( BinToInt head
  , CursorI p a o (PtrListCons head PtrListNil)
  ) =>
  Cursor p a o head where
  cursor' _ _ = FrameT $ (pure $ AudioUnitRef (toInt' (Proxy :: Proxy head)))

class CursorRes (tag :: Type) (p :: Ptr) (i :: Node) (plist :: EdgeProfile) | tag p i -> plist

instance cursorResAllpass :: CursorRes (CTOR.Allpass a b c) ptr (NodeC (AU.TAllpass ptr) edge) edge
else instance cursorResBandpass :: CursorRes (CTOR.Bandpass a b c) ptr (NodeC (AU.TBandpass ptr) edge) edge
else instance cursorResConstant :: CursorRes (CTOR.Constant a) ptr (NodeC (AU.TConstant ptr) edge) edge
else instance cursorResConvolver :: CursorRes (CTOR.Convolver a b) ptr (NodeC (AU.TConvolver ptr) edge) edge
else instance cursorResDelay :: CursorRes (CTOR.Delay a b) ptr (NodeC (AU.TDelay ptr) edge) edge
else instance cursorResDynamicsCompressor :: CursorRes (CTOR.DynamicsCompressor a b c d e f) ptr (NodeC (AU.TDynamicsCompressor ptr) edge) edge
else instance cursorResGain :: CursorRes (CTOR.Gain a b) ptr (NodeC (AU.TGain ptr) edge) edge
else instance cursorResHighpass :: CursorRes (CTOR.Highpass a b c) ptr (NodeC (AU.THighpass ptr) edge) edge
else instance cursorResHighshelf :: CursorRes (CTOR.Highshelf a b c) ptr (NodeC (AU.THighshelf ptr) edge) edge
else instance cursorResLoopBuf :: CursorRes (CTOR.LoopBuf a b) ptr (NodeC (AU.TLoopBuf ptr) edge) edge
else instance cursorResLowpass :: CursorRes (CTOR.Lowpass a b c) ptr (NodeC (AU.TLowpass ptr) edge) edge
else instance cursorResLowshelf :: CursorRes (CTOR.Lowshelf a b c) ptr (NodeC (AU.TLowshelf ptr) edge) edge
else instance cursorResMicrophone :: CursorRes (CTOR.Microphone) ptr (NodeC (AU.TMicrophone ptr) edge) edge
else instance cursorResNotch :: CursorRes (CTOR.Notch a b c) ptr (NodeC (AU.TNotch ptr) edge) edge
else instance cursorResPeaking :: CursorRes (CTOR.Peaking a b c d) ptr (NodeC (AU.TPeaking ptr) edge) edge
else instance cursorResPeriodicOsc :: CursorRes (CTOR.PeriodicOsc a b) ptr (NodeC (AU.TPeriodicOsc ptr) edge) edge
else instance cursorResPlayBuf :: CursorRes (CTOR.PlayBuf a b) ptr (NodeC (AU.TPlayBuf ptr) edge) edge
else instance cursorResRecorder :: CursorRes (CTOR.Recorder a b) ptr (NodeC (AU.TRecorder ptr) edge) edge
else instance cursorResSawtoothOsc :: CursorRes (CTOR.SawtoothOsc a) ptr (NodeC (AU.TSawtoothOsc ptr) edge) edge
else instance cursorResSinOsc :: CursorRes (CTOR.SinOsc a) ptr (NodeC (AU.TSinOsc ptr) edge) edge
else instance cursorResSpeaker :: CursorRes (CTOR.Speaker a) ptr (NodeC (AU.TSpeaker ptr) edge) edge
else instance cursorResSquareOsc :: CursorRes (CTOR.SquareOsc a) ptr (NodeC (AU.TSquareOsc ptr) edge) edge
else instance cursorResStereoPanner :: CursorRes (CTOR.StereoPanner a b) ptr (NodeC (AU.TStereoPanner ptr) edge) edge
else instance cursorResTriangleOsc :: CursorRes (CTOR.TriangleOsc a) ptr (NodeC (AU.TTriangleOsc ptr) edge) edge
else instance cursorResWaveShaper :: CursorRes (CTOR.WaveShaper a b c) ptr (NodeC (AU.TWaveShaper ptr) edge) edge
else instance cursorResMiss :: CursorRes tag p n NoEdge

class Cursor' (tag :: Type) (p :: Ptr) (i :: NodeList) (nextP :: EdgeProfile) | tag p i -> nextP

instance cursorNil :: Cursor' tag p NodeListNil NoEdge

instance cursorCons ::
  ( CursorRes tag p head headPlist
  , Cursor' tag p tail tailPlist
  , EdgeProfileChooseGreater headPlist tailPlist plist
  ) =>
  Cursor' tag p (NodeListCons head tail) plist

class CursorX (tag :: Type) (p :: Ptr) (i :: Universe) (nextP :: EdgeProfile) | tag p i -> nextP

instance cursorX ::
  ( GraphToNodeList ig il
  , Cursor' tag p il nextP
  ) =>
  CursorX tag p (UniverseC i ig cb sk) nextP

class CursorI (p :: EdgeProfile) (a :: Type) (o :: Universe) (ptr :: PtrList) | p a o -> ptr

instance cursorNoEdge :: CursorI NoEdge g inuniv PtrListNil

instance cursorSkolem :: BinToInt p => CursorI (SingleEdge p) (Proxy skolem) inuniv PtrListNil

instance cursorIdentity :: (BinToInt p, CursorI (SingleEdge p) x inuniv o) => CursorI (SingleEdge p) (Identity x) inuniv o

instance cursorFocus :: (BinToInt p, CursorI (SingleEdge p) x inuniv o) => CursorI (SingleEdge p) (Focus x) inuniv (PtrListCons p o)

instance cursorMany2 ::
  ( BinToInt p
  , BinToInt a
  , CursorI (SingleEdge p) x inuniv o0
  , CursorI (ManyEdges a b) y inuniv o1
  , PtrListAppend o0 o1 oo
  ) =>
  CursorI (ManyEdges p (PtrListCons a b)) (x /\ y) inuniv oo

instance cursorMany1 ::
  (BinToInt p, CursorI (SingleEdge p) a inuniv o) =>
  CursorI (ManyEdges p PtrListNil) (a /\ Unit) inuniv o

-- incoming to the change will be the ptr of the inner closure, which is the actual connection -- we run the inner closure to get the ptr for the outer closure
instance cursorDup ::
  ( Create
      a
      (UniverseC D0 InitialGraph changeBit (SkolemListCons (SkolemPairC skolem D0) skolems))
      (UniverseC outptr grapho changeBit (SkolemListCons (SkolemPairC skolem D0) skolems))
      ignore
  , BinToInt p
  , BinToInt outptr
  , BinToInt continuation
  , BinSub p outptr continuation
  , CursorI (SingleEdge p) b inuniv o0
  , CursorI (SingleEdge continuation) a inuniv o1
  , PtrListAppend o0 o1 oo
  ) =>
  CursorI (SingleEdge p) (CTOR.Dup a (Proxy skolem -> b)) inuniv oo

--------------
instance cursorAllpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Allpass argA argB argC) p inuniv nextP
  , CursorI nextP argC inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Allpass argA argB fOfargC) inuniv o

instance cursorBandpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Bandpass argA argB argC) p inuniv nextP
  , CursorI nextP argC inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Bandpass argA argB fOfargC) inuniv o

instance cursorConstant ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.Constant argA) inuniv PtrListNil

instance cursorConvolver ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , CursorX (CTOR.Convolver argA argB) p inuniv nextP
  , CursorI nextP argB inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Convolver argA fOfargB) inuniv o

instance cursorDelay ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , CursorX (CTOR.Delay argA argB) p inuniv nextP
  , CursorI nextP argB inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Delay argA fOfargB) inuniv o

instance cursorDynamicsCompressor ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargF skolem
  , ToSkolemizedFunction fOfargF skolem argF
  , CursorX (CTOR.DynamicsCompressor argA argB argC argD argE argF) p inuniv nextP
  , CursorI nextP argF inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.DynamicsCompressor argA argB argC argD argE fOfargF) inuniv o

instance cursorHighpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Highpass argA argB argC) p inuniv nextP
  , CursorI nextP argC inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Highpass argA argB fOfargC) inuniv o

instance cursorHighshelf ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Highshelf argA argB argC) p inuniv nextP
  , CursorI nextP argC inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Highshelf argA argB fOfargC) inuniv o

instance cursorLoopBuf ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.LoopBuf argA argB) inuniv PtrListNil

instance cursorLowpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Lowpass argA argB argC) p inuniv nextP
  , CursorI nextP argC inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Lowpass argA argB fOfargC) inuniv o

instance cursorLowshelf ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Lowshelf argA argB argC) p inuniv nextP
  , CursorI nextP argC inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Lowshelf argA argB fOfargC) inuniv o

instance cursorMicrophone ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.Microphone) inuniv PtrListNil

instance cursorNotch ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.Notch argA argB argC) p inuniv nextP
  , CursorI nextP argC inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Notch argA argB fOfargC) inuniv o

instance cursorPeaking ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargD skolem
  , ToSkolemizedFunction fOfargD skolem argD
  , CursorX (CTOR.Peaking argA argB argC argD) p inuniv nextP
  , CursorI nextP argD inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Peaking argA argB argC fOfargD) inuniv o

instance cursorPeriodicOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.PeriodicOsc argA argB) inuniv PtrListNil

instance cursorPlayBuf ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.PlayBuf argA argB) inuniv PtrListNil

instance cursorRecorder ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , CursorX (CTOR.Recorder argA argB) p inuniv nextP
  , CursorI nextP argB inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Recorder argA fOfargB) inuniv o

instance cursorSawtoothOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.SawtoothOsc argA) inuniv PtrListNil

instance cursorSinOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.SinOsc argA) inuniv PtrListNil

instance cursorSquareOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.SquareOsc argA) inuniv PtrListNil

instance cursorStereoPanner ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargB skolem
  , ToSkolemizedFunction fOfargB skolem argB
  , CursorX (CTOR.StereoPanner argA argB) p inuniv nextP
  , CursorI nextP argB inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.StereoPanner argA fOfargB) inuniv o

instance cursorTriangleOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.TriangleOsc argA) inuniv PtrListNil

instance cursorWaveShaper ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fOfargC skolem
  , ToSkolemizedFunction fOfargC skolem argC
  , CursorX (CTOR.WaveShaper argA argB argC) p inuniv nextP
  , CursorI nextP argC inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.WaveShaper argA argB fOfargC) inuniv o

---------------
instance cursorGain ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , CursorX (CTOR.Gain a b) p inuniv nextP
  , CursorI nextP b inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Gain a fb) inuniv o

instance cursorSpeaker ::
  ( BinToInt p
  , CursorX (CTOR.Speaker a) p inuniv nextP
  , CursorI nextP a inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.Speaker a) inuniv o
