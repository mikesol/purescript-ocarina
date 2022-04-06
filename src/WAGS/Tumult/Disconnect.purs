module WAGS.Tumult.Disconnect where

import Prelude hiding (Ordering(..))

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import Type.Proxy (Proxy(..))
import WAGS.Tumult.Control.Types (WAG(..))
import WAGS.Tumult.Graph.AudioUnit (class TypeToSym)
import WAGS.Tumult.Graph.Graph (Graph)
import WAGS.Tumult.Graph.Node (NodeC)
import WAGS.Tumult.Instructions as I

-- | Disconnect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class
  Disconnect (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph)
  | source dest i -> o where
  disconnect
    :: forall proxy
     . { source :: proxy source, dest :: proxy dest }
    -> WAG i
    -> WAG o

instance disconnector ::
  ( IsSymbol from
  , IsSymbol to
  , TypeToSym fromN fromSym
  , TypeToSym toN toSym
  , IsSymbol fromSym
  , IsSymbol toSym
  , R.Cons from (NodeC fromN ignoreEdges) ignore1 graphi
  , R.Cons to (NodeC toN { | e }) newg graphi
  , R.Cons from Unit e' e
  , R.Lacks from e'
  , R.Cons to (NodeC toN { | e' }) newg grapho
  ) =>
  Disconnect from to graphi grapho where
  disconnect { source, dest } w =
    WAG
      { instructions: instructions <>
          [ I.iDisconnectXFromY
              { from
              , fromUnit: reflectSymbol (Proxy :: _ fromSym)
              , to
              , toUnit: reflectSymbol (Proxy :: _ toSym)
              }
          ]
      }
    where
    WAG { instructions } = w

    from = reflectSymbol source

    to = reflectSymbol dest
