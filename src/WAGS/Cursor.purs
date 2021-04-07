module WAGS.Cursor where

import Prelude
import Data.Identity (Identity)
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Frame(..))
import WAGS.Create (class Create)
import WAGS.Graph.Constructors (Dup, Gain, Highpass, SinOsc, Speaker)
import WAGS.Graph.Decorators (Focus)
import WAGS.Universe.AudioUnit (AudioUnitRef(..), TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Universe.Bin (class BinSub, class BinToInt, D0, Ptr, PtrList, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, InitialGraph)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class ToSkolemizedFunction, SkolemListCons, SkolemPairC)
import WAGS.Universe.Universe (Universe, UniverseC)
import WAGS.Validation (class EdgeProfileChooseGreater, class PtrListAppend, class TerminalIdentityEdge)

cursor ::
  forall edge a q r s t env proof p.
  TerminalIdentityEdge r edge =>
  Cursor edge a (UniverseC q r s t) p =>
  BinToInt p =>
  a -> Frame env proof (UniverseC q r s t) (UniverseC q r s t) (AudioUnitRef p)
cursor = cursor' (Proxy :: _ edge)

class Cursor (p :: EdgeProfile) (a :: Type) (o :: Universe) (ptr :: Ptr) | p a o -> ptr where
  cursor' :: forall env proof. Proxy p -> a -> Frame env proof o o (AudioUnitRef ptr)

instance cursorRecurse ::
  ( BinToInt head
  , CursorI p a o (PtrListCons head PtrListNil)
  ) =>
  Cursor p a o head where
  cursor' _ _ = Frame $ (pure $ AudioUnitRef (toInt' (Proxy :: Proxy head)))

class CursorRes (tag :: Type) (p :: Ptr) (i :: Node) (plist :: EdgeProfile) | tag p i -> plist

instance cursorResSinOsc :: CursorRes (SinOsc a) p (NodeC (TSinOsc p) e) e
else instance cursorResHighpass :: CursorRes (Highpass a b c) p (NodeC (THighpass p) e) e
else instance cursorResGain :: CursorRes (Gain a b) p (NodeC (TGain p) e) e
else instance cursorResSpeaker :: CursorRes (Speaker a) p (NodeC (TSpeaker p) e) e
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
  CursorI (SingleEdge p) (Dup a (Proxy skolem -> b)) inuniv oo

instance cursorSinOsc ::
  BinToInt p =>
  CursorI (SingleEdge p) (SinOsc a) inuniv PtrListNil

instance cursorHighpass ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , CursorX (Highpass a b c) p inuniv nextP
  , CursorI nextP c inuniv o
  ) =>
  CursorI (SingleEdge p) (Highpass a b fc) inuniv o

instance cursorGain ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , CursorX (Gain a b) p inuniv nextP
  , CursorI nextP b inuniv o
  ) =>
  CursorI (SingleEdge p) (Gain a fb) inuniv o

instance cursorSpeaker ::
  ( BinToInt p
  , CursorX (Speaker a) p inuniv nextP
  , CursorI nextP a inuniv o
  ) =>
  CursorI (SingleEdge p) (Speaker a) inuniv o
