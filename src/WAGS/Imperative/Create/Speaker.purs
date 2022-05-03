module WAGS.Imperative.Create.Speaker where

import Prelude

import FRP.Event.Class (bang)
import Prim.Boolean (True)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

speaker
  :: forall p i o id
   . IsSymbol id
  => CreateNode i id True o
  => Proxy id
  -> GraphBuilder p i o (T.GraphUnit id T.Speaker)
speaker _ = GraphBuilder go
  where
  go (Core.AudioInterpret { makeSpeaker }) =
    { event: bang $ makeSpeaker { id: reflectSymbol (Proxy :: _ id) }
    , result: T.GraphUnit
    }
