module WAGS.Tumult.Tumult.Make where

import Data.Set (empty)
import Prim.RowList (class RowToList)
import WAGS.Tumult.Control.Types (WAG(..))
import WAGS.Tumult.Create (class Create, create)
import WAGS.Tumult.Tumult (Tumultuous, unsafeTumult)
import WAGS.Tumult.Validation (class SubgraphIsRenderable)

tumultuously
  :: forall terminus inputs scene graph graphRL
   . RowToList graph graphRL
  => Create scene () graph
  => SubgraphIsRenderable graph terminus inputs
  => { | scene }
  -> Tumultuous terminus inputs
tumultuously scene = unsafeTumult instructions
  where
  WAG { instructions } = create scene (WAG { instructions: empty } :: WAG ())
