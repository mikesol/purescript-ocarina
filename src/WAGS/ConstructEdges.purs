module WAGS.ConstructEdges where

import Prelude
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row as Row
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (class TypeToSym)
import WAGS.Util (class AutoIncrementingInsert, class NatToSym)

class ConstructEdges (suffix :: Symbol) (map :: Type) a (newSuffix :: Symbol) (newMap :: Type) b | suffix map a -> newSuffix newMap b where
  constructEdges :: Proxy suffix -> Proxy map -> a -> (Proxy newSuffix /\ Proxy newMap /\ b)

instance constructEdgesTuple :: ConstructEdges suffix map (a /\ { | b }) "" Unit (a /\ { | b }) where
  constructEdges _ _ a = Proxy /\ Proxy /\ a
else instance constructEdgesNormVal ::
  ( TypeToSym b bSym
  , AutoIncrementingInsert b map val newMap
  , NatToSym val valSym
  , Sym.Append suffix "_" step0
  , Sym.Append step0 bSym step1
  , Sym.Append step1 "_" step2
  , Sym.Append step2 valSym newSym
  , IsSymbol newSym
  , Row.Lacks newSym ()
  , Row.Cons newSym b () c
  ) =>
  ConstructEdges suffix map (a /\ b) suffix newMap (a /\ { | c }) where
  constructEdges _ _ (a /\ b) = Proxy /\ Proxy /\ (a /\ Record.insert (Proxy :: _ newSym) b {})
else instance constructEdgesRest :: ConstructEdges suffix map a suffix map (a /\ {}) where
  constructEdges _ _ a = Proxy /\ Proxy /\ (a /\ {})

class ConstructEdgesT (suffix :: Symbol) (map :: Type) (a :: Type) (newSuffix :: Symbol) (newMap :: Type) (b :: Type) | suffix map a -> newSuffix newMap b

instance constructEdgesTTuple :: ConstructEdgesT suffix map (a /\ { | b }) "" Unit (a /\ { | b })
else instance constructEdgesTRest :: ConstructEdgesT suffix map a suffix map (a /\ {})
