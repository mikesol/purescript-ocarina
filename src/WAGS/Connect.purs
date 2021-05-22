module WAGS.Connect where

import Prelude hiding (Ordering(..))
import Control.Monad.State (modify_)
import Data.Map as M
import Data.Set as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import Prim.Row as R
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Interpret (class AudioInterpret, connectXToY)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)

-- | Connect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class Connect (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph) | source dest i -> o where
  connect :: forall proxy env audio engine proof m res. Monad m => AudioInterpret audio engine => proxy source -> proxy dest -> FrameT env audio engine proof m res { | i } { | o } Unit

instance connectInstance ::
  ( IsSymbol from
  , IsSymbol to
  , R.Cons from ignore0 ignore1 graphi
  , R.Cons to (NodeC n { | e }) newg graphi
  , R.Lacks from e
  , R.Cons from Unit e e'
  , R.Cons to (NodeC n { | e' }) newg grapho
  ) =>
  Connect from to graphi grapho where
  connect fromI' toI' =
    unsafeFrame
      $ do
          modify_
            ( \i ->
                i
                  { internalEdges = M.insertWith S.union toI (S.singleton fromI) i.internalEdges
                  , instructions = i.instructions <> [ connectXToY fromI toI ]
                  }
            )
    where
    fromI = reflectSymbol fromI'

    toI = reflectSymbol toI'

data ConnectFoldingWithIndex
  = ConnectFoldingWithIndex

instance connectFoldingWithIndex ::
  ( Monad m
  , AudioInterpret audio engine
  , Connect from to midGraph outGraph
  , IsSymbol from
  , IsSymbol to
  ) =>
  FoldingWithIndex
    ConnectFoldingWithIndex
    (proxy from)
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | midGraph }
        (proxy to)
    )
    anything
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | outGraph }
        (proxy to)
    ) where
  foldingWithIndex ConnectFoldingWithIndex from ifr a =
    WAGS.bind
      ifr
      (\to -> connect from to $> to)
