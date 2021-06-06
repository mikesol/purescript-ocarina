module WAGS.Disconnect where

import Prelude hiding (Ordering(..))
import Data.Functor (voidRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Interpret (class AudioInterpret, disconnectXFromY)

idisconnect ::
  forall proxy source dest audio engine proof res i o.
  AudioInterpret audio engine =>
  Disconnect source dest i o =>
  { source :: proxy source, dest :: proxy dest } ->
  IxWAG audio engine proof res { | i } { | o } Unit
idisconnect ptrs = IxWAG (disconnect <<< voidRight ptrs)

-- | Disconnect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class Disconnect (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph) | source dest i -> o where
  disconnect ::
    forall proxy audio engine proof res.
    AudioInterpret audio engine =>
    WAG audio engine proof res { | i } { source :: proxy source, dest :: proxy dest } ->
    WAG audio engine proof res { | o } Unit

instance disconnector ::
  ( IsSymbol from
  , IsSymbol to
  , R.Cons from ignore0 ignore1 graphi
  , R.Cons to (NodeC n { | e }) newg graphi
  , R.Cons from Unit e' e
  , R.Lacks from e'
  , R.Cons to (NodeC n { | e' }) newg grapho
  ) =>
  Disconnect from to graphi grapho where
  disconnect w =
    unsafeWAG
      { context:
          i
            { instructions = i.instructions <> [ disconnectXFromY fromI toI ]
            }
      , value: unit
      }
    where
    { context: i, value: { source: fromI', dest: toI' } } = unsafeUnWAG w

    fromI = reflectSymbol fromI'

    toI = reflectSymbol toI'
