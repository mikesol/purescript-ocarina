module WAGS.Tumult.Make where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.Typelevel.Num (class Pos)
import Data.Vec (Vec, toArray)
import Prim.RowList (class RowToList)
import WAGS.Control.Functions (start)
import WAGS.Control.Functions.Subgraph ((@||>), freeze)
import WAGS.Control.Types (Frame0, SubScene, WAG, oneSubFrame)
import WAGS.Create (class Create, create)
import WAGS.Rendered (Instruction)
import WAGS.Tumult (Tumultuous, unsafeTumult)
import WAGS.Validation (class NodesCanBeTumultuous, class SubgraphIsRenderable)

makeTumult
  :: forall n terminus inputs scene graph graphRL
   . Pos n
  => RowToList graph graphRL
  => Create scene () graph
  => SubgraphIsRenderable graph terminus inputs
  => NodesCanBeTumultuous graphRL
  => Vec n { | scene }
  -> Tumultuous n terminus inputs
makeTumult scenes = unsafeTumult (map go (toArray scenes))
  where
  go :: { | scene } -> Set Instruction
  go scene = Set.fromFoldable $ map ((#) unit) tmt
    where
    init :: WAG Unit Instruction Frame0 Unit graph Unit
    init = create (start $> scene)
    subscene :: SubScene terminus inputs Unit Unit Instruction Frame0 Unit
    subscene = init @||> freeze
    frame = oneSubFrame subscene unit
    tmt = frame.instructions


