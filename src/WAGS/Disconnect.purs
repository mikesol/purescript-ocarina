module WAGS.Disconnect where

import Prelude hiding (Ordering(..))
import Control.Monad.State (modify_)
import Data.Map as M
import Data.Set as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as R
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Interpret (class AudioInterpret, disconnectXFromY)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)

-- | Disconnect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class Disconnect (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph) | source dest i -> o where
  disconnect :: forall proxy env audio engine proof m res. Monad m => AudioInterpret audio engine => proxy source -> proxy dest -> FrameT env audio engine proof m res { | i } { | o }  Unit

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
  disconnect fromI' toI' =
    unsafeFrame
      $ do
          modify_
            ( \i ->
                i
                  { internalEdges = M.insertWith S.difference toI (S.singleton fromI) (i.internalEdges)
                  , instructions = i.instructions <> [ disconnectXFromY fromI toI ]
                  }
            )
    where
    fromI = reflectSymbol fromI'

    toI = reflectSymbol toI'
