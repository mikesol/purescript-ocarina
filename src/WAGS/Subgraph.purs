module Deku.Subgraph where

import Prelude

import Control.Alt ((<|>))
import Data.Hashable (class Hashable, hash)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Core (Input(..), Subgraph)
import Deku.Core as C
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

data SubgraphAction env
  = InsertOrUpdate env
  | Remove

class MakeInputs :: forall k. k -> Row Type -> Constraint
class MakeInputs consumedRL consumed | consumedRL -> consumed where
  inputs :: forall proxy. proxy consumedRL -> { | consumed }

instance inputsNil :: MakeInputs (RL.Nil) () where
  inputs _ = {}

instance inputsCons ::
  ( IsSymbol key
  , Row.Cons key Input consumed' consumed
  , Row.Lacks key consumed'
  , MakeInputs rest consumed'
  ) =>
  MakeInputs (RL.Cons key Input rest) consumed where
  inputs _ = let px = (Proxy :: _ key) in Record.insert px (Input (reflectSymbol px)) (inputs (Proxy :: _ rest))

subgraph
  :: forall index env outputChannels consumed consumedRL produced event payload
   . IsEvent event
  => Hashable index
  => RowToList consumed consumedRL
  => MakeInputs consumedRL consumed
  => event (index /\ SubgraphAction env)
  -> ({ | consumed } -> Subgraph index env outputChannels event payload)
  -> C.Node outputChannels produced consumed event payload
subgraph mods elt = C.Node go
  where
  subg = elt (inputs (Proxy :: _ consumedRL))
  go parent (C.AudioInterpret { ids, makeSubgraph, insertOrUpdateSubgraph, removeSubgraph }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeSubgraph
                { id: me, parent: parent, scenes: subg }
            )
            <|> map
              ( \(index /\ instr) -> case instr of
                  Remove -> removeSubgraph { id: me, pos: hash index, index }
                  InsertOrUpdate env -> insertOrUpdateSubgraph
                    { id: me, pos: hash index, index, env }
              )
              mods
      )

subgraph'
  :: forall proxy sym index env outputChannels consumed' consumed consumedRL produced event payload
   . IsEvent event
  => Hashable index
  => RowToList consumed consumedRL
  => MakeInputs consumedRL consumed
  => Row.Cons sym C.Input consumed' consumed
  => proxy sym
  -> event (index /\ SubgraphAction env)
  -> ({ | consumed } -> Subgraph index env outputChannels event payload)
  -> C.Node outputChannels produced consumed event payload
subgraph' _ mods elt = let C.Node n = subgraph mods elt in C.Node n

infixr 6 subgraph as @@