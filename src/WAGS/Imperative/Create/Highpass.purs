module WAGS.Imperative.Create.Highpass where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Highpass as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

highpass
  :: forall l p i o id initialHighpass
   . IsSymbol id
  => Parameters.InitialHighpass initialHighpass
  => CreateNode i id False o
  => Proxy id
  -> initialHighpass
  -> Event (Core.Highpass l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Highpass)
highpass _ initialHighpass attributes = GraphBuilder go
  where
  initializeHighpass = unwrap $ Parameters.toInitializeHighpass initialHighpass
  go i@(Core.AudioInterpret { makeHighpass, setFrequency, setQ }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeHighpass
            { id
            , parent: nothing
            , scope: "imperative"
            , frequency: initializeHighpass.frequency
            , q: initializeHighpass.q
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { frequency: tmpResolveAU "imperative" i $ setFrequency <<< { id, frequency: _ }
            , q: tmpResolveAU "imperative" i $ setQ <<< { id, q: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
