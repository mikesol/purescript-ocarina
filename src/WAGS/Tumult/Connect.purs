module WAGS.Tumult.Connect where

import Prelude hiding (Ordering(..))

import Data.Set (insert)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import Type.Proxy (Proxy(..))
import WAGS.Tumult.Control.Types (WAG(..))
import WAGS.Tumult.Graph.AudioUnit (class TypeToSym)
import WAGS.Tumult.Graph.Graph (Graph)
import WAGS.Tumult.Graph.Node (NodeC)
import WAGS.Tumult.Instructions as I

-- | Connect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class
  Connect (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph)
  | source dest i -> o where
  connect
    :: forall proxy
     . { source :: proxy source, dest :: proxy dest }
    -> WAG i
    -> WAG o

instance connectInstance ::
  ( IsSymbol from
  , IsSymbol to
  , TypeToSym fromN fromSym
  , TypeToSym toN toSym
  , IsSymbol fromSym
  , IsSymbol toSym
  , R.Cons from (NodeC fromN ignoreEdges) ignore1 graphi
  , R.Cons to (NodeC toN { | e }) newg graphi
  , R.Lacks from e
  , R.Cons from Unit e e'
  , R.Cons to (NodeC toN { | e' }) newg grapho
  ) =>
  Connect from to graphi grapho where
  connect { source: fromI', dest: toI' } w =
    WAG
      { instructions: insert
          ( I.iConnectXToY
              { from
              , fromUnit: reflectSymbol (Proxy :: _ fromSym)
              , to
              , toUnit: reflectSymbol (Proxy :: _ toSym)
              }
          )
          instructions

      }
    where
    WAG { instructions } = w

    from = reflectSymbol fromI'

    to = reflectSymbol toI'

class
  ConnectT (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph)
  | source dest i -> o

instance connectTInstance ::
  ( R.Cons from ignore0 ignore1 graphi
  , R.Cons to (NodeC n { | e }) newg graphi
  , R.Lacks from e
  , R.Cons from Unit e e'
  , R.Cons to (NodeC n { | e' }) newg grapho
  ) =>
  ConnectT from to graphi grapho