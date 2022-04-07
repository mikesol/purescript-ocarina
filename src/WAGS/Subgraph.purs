module WAGS.Subgraph where

import Prelude

import Control.Alt ((<|>))
import Data.Hashable (class Hashable, hash)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant.Maybe (Maybe, just, nothing)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Control (__mId)
import WAGS.Core (Input(..), Subgraph)
import WAGS.Core as C

data SubgraphAction env
  = InsertOrUpdate env
  | Remove

class MakeInputs :: forall k. k -> Row Type -> Constraint
class MakeInputs consumedRL consumed | consumedRL -> consumed where
  inputs :: forall proxy. String -> proxy consumedRL -> { | consumed }

instance inputsNil :: MakeInputs (RL.Nil) () where
  inputs _ _ = {}

instance inputsCons ::
  ( IsSymbol key
  , Row.Cons key Input consumed' consumed
  , Row.Lacks key consumed'
  , MakeInputs rest consumed'
  ) =>
  MakeInputs (RL.Cons key Input rest) consumed where
  inputs scope _ =
    let
      px = (Proxy :: _ key)
    in
      Record.insert px (Input (reflectSymbol px <> "!" <> scope)) (inputs scope (Proxy :: _ rest))

__subgraph
  :: forall index env outputChannels produced consumed consumedRL sgProduced
       sgConsumed event proof payload
   . IsEvent (event proof)
  => Hashable index
  => RowToList consumed consumedRL
  => MakeInputs consumedRL consumed
  => Maybe String
  -> event proof (index /\ SubgraphAction env)
  -> ( { | consumed }
       -> Subgraph index env outputChannels sgProduced sgConsumed event payload
     )
  -> C.Node outputChannels produced consumed event proof payload
__subgraph mId mods elt = C.Node go
  where
  go
    parent
    ( C.AudioInterpret
        { ids, scope, makeSubgraph, insertOrUpdateSubgraph, removeSubgraph }
    ) =
    let
      subg = elt (inputs scope (Proxy :: _ consumedRL))
    in
      keepLatest
        ( (sample_ ids (pure unit)) <#> __mId scope mId \me ->
            pure
              ( makeSubgraph
                  { id: me, parent: parent, scenes: subg, scope }
              )
              <|> map
                ( \(index /\ instr) -> case instr of
                    Remove -> removeSubgraph { id: me, pos: hash index, index }
                    InsertOrUpdate env -> insertOrUpdateSubgraph
                      { id: me, pos: hash index, index, env }
                )
                mods
        )

subgraph
  :: forall index env outputChannels consumed consumedRL sgProduced
       sgConsumed event proof payload
   . IsEvent (event proof)
  => Hashable index
  => RowToList consumed consumedRL
  => MakeInputs consumedRL consumed
  => event proof (index /\ SubgraphAction env)
  -> ( { | consumed }
       -> Subgraph index env outputChannels sgProduced sgConsumed event payload
     )
  -> C.Node outputChannels () consumed event proof payload
subgraph = __subgraph nothing

subgraph'
  :: forall proxy sym index env outputChannels produced consumed consumedRL
       sgProduced
       sgConsumed event proof payload
   . IsEvent (event proof)
  => Hashable index
  => RowToList consumed consumedRL
  => MakeInputs consumedRL consumed
  => IsSymbol sym
  => Row.Cons sym C.Input () produced
  => proxy sym
  -> event proof (index /\ SubgraphAction env)
  -> ( { | consumed }
       -> Subgraph index env outputChannels sgProduced sgConsumed event payload
     )
  -> C.Node outputChannels () consumed event proof payload
subgraph' px = __subgraph (just (reflectSymbol px))

infixr 6 subgraph as @@