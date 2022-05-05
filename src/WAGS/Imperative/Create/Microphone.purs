module WAGS.Imperative.Create.Microphone where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant.Maybe (nothing)
import FRP.Event (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Microphone as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

microphone
  :: forall p i o id initialMicrophone
   . IsSymbol id
  => Parameters.InitialMicrophone initialMicrophone
  => CreateNode i id False o
  => Proxy id
  -> initialMicrophone
  -> GraphBuilder p i o (T.GraphUnit id T.MediaElement)
microphone _ initialMicrophone = GraphBuilder go
  where
  { microphone } = unwrap $ Parameters.toInitializeMicrophone initialMicrophone
  go (Core.AudioInterpret { makeMicrophone }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
        in
          bang $ makeMicrophone
            { id
            , parent: nothing
            , scope: "imperative"
            , microphone
            }
    , result: T.GraphUnit
    }
