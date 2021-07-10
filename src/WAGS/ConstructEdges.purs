module WAGS.ConstructEdges where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row as Row
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Edgeable (class EdgeableT)
import WAGS.Graph.AudioUnit (class TypeToSym)
import WAGS.Util (class AutoIncrementingInsert, class NatToSym)

class ConstructEdges (prefix :: Symbol) (map :: Type) a (newPrefix :: Type) (newMap :: Type) b | prefix map a -> newPrefix newMap b where
  constructEdges :: Proxy prefix -> Proxy map -> a -> (Proxy newPrefix /\ Proxy newMap /\ b)

instance constructEdgesTuple :: ConstructEdges prefix map (a /\ { | b }) Unit Unit (a /\ { | b }) where
  constructEdges _ _ a = Proxy /\ Proxy /\ a
else instance constructEdgesNormVal ::
  ( TypeToSym b bSym
  , EdgeableT b (nd /\ eg)
  , AutoIncrementingInsert nd map val newMap
  , NatToSym val valSym
  , Sym.Append bSym "_" step2
  , Sym.Append step2 valSym newSym
  , IsSymbol newSym
  , Row.Lacks newSym ()
  , Row.Cons newSym b () c
  ) =>
  ConstructEdges prefix map (a /\ b) (Proxy prefix) newMap (a /\ { | c }) where
  constructEdges _ _ (a /\ b) = Proxy /\ Proxy /\ (a /\ Record.insert (Proxy :: _ newSym) b {})
else instance constructEdgesRest :: ConstructEdges prefix map a (Proxy prefix) map (a /\ {}) where
  constructEdges _ _ a = Proxy /\ Proxy /\ (a /\ {})

class ConstructEdgesT (prefix :: Symbol) (map :: Type) (a :: Type) (newPrefix :: Type) (newMap :: Type) (b :: Type) | prefix map a -> newPrefix newMap b

instance constructEdgesTTuple :: ConstructEdgesT prefix map (a /\ { | b }) Unit Unit (a /\ { | b })
else instance constructEdgesTNormVal ::
  ( TypeToSym b bSym
  , EdgeableT b (nd /\ eg)
  , AutoIncrementingInsert nd map val newMap
  , NatToSym val valSym
  , Sym.Append bSym "_" step2
  , Sym.Append step2 valSym newSym
  , Row.Lacks newSym ()
  , Row.Cons newSym b () c
  ) =>
  ConstructEdgesT prefix map (a /\ b) (Proxy prefix) newMap (a /\ { | c })
else instance constructEdgesTRest :: ConstructEdgesT prefix map a (Proxy prefix) map (a /\ {})
