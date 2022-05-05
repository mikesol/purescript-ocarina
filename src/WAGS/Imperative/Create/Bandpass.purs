module WAGS.Imperative.Create.Bandpass where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Bandpass as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

bandpass
  :: forall l p i o id initialBandpass
   . IsSymbol id
  => Parameters.InitialBandpass initialBandpass
  => CreateNode i id False o
  => Proxy id
  -> initialBandpass
  -> Event (Core.Bandpass l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Bandpass)
bandpass _ initialBandpass attributes = GraphBuilder go
  where
  { frequency, q } = unwrap $ Parameters.toInitializeBandpass initialBandpass
  go i@(Core.AudioInterpret { makeBandpass, setFrequency, setQ }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeBandpass
            { id
            , parent: nothing
            , scope: "imperative"
            , frequency
            , q
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { frequency: tmpResolveAU "imperative" i $ setFrequency <<< { id, frequency: _ }
            , q: tmpResolveAU "imperative" i $ setQ <<< { id, q: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
