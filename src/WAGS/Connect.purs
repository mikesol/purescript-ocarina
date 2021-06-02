module WAGS.Connect where

import Prelude hiding (Ordering(..))

import Control.Comonad (extract)
import Data.Functor (voidRight)
import Data.Map as M
import Data.Set as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class FoldingWithIndex)
import Prim.Row as R
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Interpret (class AudioInterpret, connectXToY)

iconnect ::
  forall proxy source dest audio engine proof res i o.
  AudioInterpret audio engine =>
  Connect source dest i o =>
  { source :: proxy source, dest :: proxy dest } ->
  IxWAG audio engine proof res { | i } { | o } Unit
iconnect ptrs = IxWAG (connect <<< voidRight ptrs)

-- | Connect node `source` from node `dest` in graph `i`, resulting in output graph `o`.
class Connect (source :: Symbol) (dest :: Symbol) (i :: Graph) (o :: Graph) | source dest i -> o where
  connect ::
    forall proxy audio engine proof res.
    AudioInterpret audio engine =>
    WAG audio engine proof res { | i } { source :: proxy source, dest :: proxy dest } ->
    WAG audio engine proof res { | o } Unit

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
  connect w =
    unsafeWAG
      { context:
          i
            { internalEdges = M.insertWith S.union toI (S.singleton fromI) i.internalEdges
            , instructions = i.instructions <> [ connectXToY fromI toI ]
            }
      , value: unit
      }
    where
    { context: i, value: { source: fromI', dest: toI' } } = unsafeUnWAG w

    fromI = reflectSymbol fromI'

    toI = reflectSymbol toI'

data ConnectFoldingWithIndex
  = ConnectFoldingWithIndex

instance connectFoldingWithIndex ::
  ( AudioInterpret audio engine
  , Connect from to inGraph outGraph
  , IsSymbol from
  , IsSymbol to
  ) =>
  FoldingWithIndex
    ConnectFoldingWithIndex
    (proxy from)
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        (proxy to)
    )
    anything
    ( WAG
        audio
        engine
        proof
        res
        { | outGraph }
        (proxy to)
    ) where
  foldingWithIndex ConnectFoldingWithIndex from ifr a = connect (ifr $> { source: from, dest: extract ifr }) $> extract ifr
