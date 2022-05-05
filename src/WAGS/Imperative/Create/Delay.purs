module WAGS.Imperative.Create.Delay where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Delay as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

delay
  :: forall l p i o id initialDelay
   . IsSymbol id
  => Parameters.InitialDelay initialDelay
  => CreateNode i id False o
  => Proxy id
  -> initialDelay
  -> Event (Core.Delay l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Delay)
delay _ initialDelay attributes = GraphBuilder go
  where
  { delayTime, maxDelayTime } = unwrap $ Parameters.toInitializeDelay initialDelay
  go i@(Core.AudioInterpret { makeDelay, setDelay }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeDelay
            { id
            , parent: nothing
            , scope: "imperative"
            , delayTime
            , maxDelayTime
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { delayTime: tmpResolveAU "imperative" i $ setDelay <<< { id, delayTime: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
