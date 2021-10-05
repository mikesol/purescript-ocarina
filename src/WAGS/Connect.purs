module WAGS.Connect where

import Prelude hiding (Ordering(..))

import Data.Functor (voidRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import Type.Proxy (Proxy(..))
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.AudioUnit (class TypeToSym)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Interpret (class AudioInterpret, connectXToY)

iconnect
  :: forall proxy source dest audio engine proof res i o
   . AudioInterpret audio engine
  => Connect source dest i o
  => { source :: proxy source, dest :: proxy dest }
  -> IxWAG audio engine proof res i o Unit
iconnect ptrs = IxWAG (connect <<< voidRight ptrs)

-- | Connect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class Connect (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph) | source dest i -> o where
  connect
    :: forall proxy audio engine proof res
     . AudioInterpret audio engine
    => WAG audio engine proof res i { source :: proxy source, dest :: proxy dest }
    -> WAG audio engine proof res o Unit

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
  connect w =
    unsafeWAG
      { context:
          i
            { instructions = i.instructions <>
                [ connectXToY
                    fromI
                    (reflectSymbol (Proxy :: _ fromSym))
                    toI
                    (reflectSymbol (Proxy :: _ toSym))
                ]
            }
      , value: unit
      }
    where
    { context: i, value: { source: fromI', dest: toI' } } = unsafeUnWAG w

    fromI = reflectSymbol fromI'

    toI = reflectSymbol toI'

class ConnectT (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph) | source dest i -> o

instance connectTInstance ::
  ( R.Cons from ignore0 ignore1 graphi
  , R.Cons to (NodeC n { | e }) newg graphi
  , R.Lacks from e
  , R.Cons from Unit e e'
  , R.Cons to (NodeC n { | e' }) newg grapho
  ) =>
  ConnectT from to graphi grapho