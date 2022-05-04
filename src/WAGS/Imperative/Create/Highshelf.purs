module WAGS.Imperative.Create.Highshelf where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Highshelf as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

highshelf
  :: forall l p i o id initialHighshelf
   . IsSymbol id
  => Parameters.InitialHighshelf initialHighshelf
  => CreateNode i id False o
  => Proxy id
  -> initialHighshelf
  -> Event (Core.Highshelf l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Highshelf)
highshelf _ initialHighshelf attributes = GraphBuilder go
  where
  initializeHighshelf = unwrap $ Parameters.toInitializeHighshelf initialHighshelf
  go i@(Core.AudioInterpret { makeHighshelf, setFrequency, setGain }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeHighshelf
            { id
            , parent: nothing
            , scope: "imperative"
            , frequency: initializeHighshelf.frequency
            , gain: initializeHighshelf.gain
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { frequency: tmpResolveAU "imperative" i $ setFrequency <<< { id, frequency: _ }
            , gain: tmpResolveAU "imperative" i $ setGain <<< { id, gain: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
