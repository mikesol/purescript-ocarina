module WAGS.Imperative.Create.Peaking where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Peaking as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

peaking
  :: forall l p i o id initialPeaking
   . IsSymbol id
  => Parameters.InitialPeaking initialPeaking
  => CreateNode i id False o
  => Proxy id
  -> initialPeaking
  -> Event (Core.Peaking l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Peaking)
peaking _ initialPeaking attributes = GraphBuilder go
  where
  { frequency, q, gain } = unwrap $ Parameters.toInitializePeaking initialPeaking
  go i@(Core.AudioInterpret { makePeaking, setFrequency, setQ, setGain }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makePeaking
            { id
            , parent: nothing
            , scope: "imperative"
            , frequency
            , q
            , gain
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { frequency: tmpResolveAU "imperative" i $ setFrequency <<< { id, frequency: _ }
            , q: tmpResolveAU "imperative" i $ setQ <<< { id, q: _ }
            , gain: tmpResolveAU "imperative" i $ setGain <<< { id, gain: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
