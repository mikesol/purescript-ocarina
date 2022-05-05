module WAGS.Imperative.Create.MediaElement where

import Prelude

import Data.Variant.Maybe (nothing)
import FRP.Event (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

mediaElement
  :: forall p i o id
   . IsSymbol id
  => CreateNode i id False o
  => Proxy id
  -> Core.InitializeMediaElement
  -> GraphBuilder p i o (T.GraphUnit id T.MediaElement)
mediaElement _ (Core.InitializeMediaElement { element }) = GraphBuilder go
  where
  go (Core.AudioInterpret { makeMediaElement }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
        in
          bang $ makeMediaElement
            { id
            , parent: nothing
            , scope: "imperative"
            , element
            }
    , result: T.GraphUnit
    }
