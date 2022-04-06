module WAGS.Tumult.Tumult.Make where

import Prelude

import Data.Typelevel.Num (class Pos)
import Data.Vec (Vec, toArray)
import Prim.RowList (class RowToList)
import WAGS.Core (Instruction)
import WAGS.Tumult.Control.Types (WAG(..))
import WAGS.Tumult.Create (class Create, create)
import WAGS.Tumult.Tumult (Tumultuous, unsafeTumult)
import WAGS.Tumult.Validation (class SubgraphIsRenderable)

tumultuously
  :: forall n terminus inputs scene graph graphRL
   . Pos n
  => RowToList graph graphRL
  => Create scene () graph
  => SubgraphIsRenderable graph terminus inputs
  => Vec n { | scene }
  -> Tumultuous n terminus inputs
tumultuously scenes = unsafeTumult (map go (toArray scenes))
  where
  go :: { | scene } -> Array Instruction
  go scene = instructions
    where
    WAG { instructions } = create scene (WAG { instructions: [] } :: WAG ())
