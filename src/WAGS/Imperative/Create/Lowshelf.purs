module WAGS.Imperative.Create.Lowshelf where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Lowshelf as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

lowshelf
  :: forall l p i o id initialLowshelf
   . IsSymbol id
  => Parameters.InitialLowshelf initialLowshelf
  => CreateNode i id False o
  => Proxy id
  -> initialLowshelf
  -> Event (Core.Lowshelf l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Lowshelf)
lowshelf _ initialLowshelf attributes = GraphBuilder go
  where
  initializeLowshelf = unwrap $ Parameters.toInitializeLowshelf initialLowshelf
  go i@(Core.AudioInterpret { makeLowshelf, setFrequency, setGain }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeLowshelf
            { id
            , parent: nothing
            , scope: "imperative"
            , frequency: initializeLowshelf.frequency
            , gain: initializeLowshelf.gain
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { frequency: tmpResolveAU "imperative" i $ setFrequency <<< { id, frequency: _ }
            , gain: tmpResolveAU "imperative" i $ setGain <<< { id, gain: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
