module WAGS.Tumult.Connect where

import Prelude hiding (Ordering(..))

import Data.Maybe (Maybe(..))
import Data.Set (insert)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import Type.Proxy (Proxy(..))
import WAGS.Tumult.Control.Types (WAG(..))
import WAGS.Tumult.Graph.AudioUnit (class TypeToSym)
import WAGS.Tumult.Graph.Graph (Graph)
import WAGS.Tumult.Graph.Node (NodeC)
import WAGS.Tumult.Instructions as I

__inputMonicker = "@!@INP(UT@`@" :: String

-- | Connect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class
  Connect (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph)
  | source dest i -> o where
  connect
    :: forall proxy
     -- this is a manual override for the source node
     -- we need this if we are working with an Input
     -- inputs are just phantom nodes, which means that whatever they are
     -- in the tumult graph is not used
     -- so we emit an instruction for the phantom node
     . Maybe String
    -> { source :: proxy source, dest :: proxy dest }
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
  connect ms { source: fromI', dest: toI' } w =
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

    from = case ms of
      Just s -> __inputMonicker <> s
      Nothing -> reflectSymbol fromI'

    to = reflectSymbol toI'
