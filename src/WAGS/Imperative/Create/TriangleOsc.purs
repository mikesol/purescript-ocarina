module WAGS.Imperative.Create.TriangleOsc where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common as Common
import WAGS.Common.Parameters.TriangleOsc as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

triangleOsc
  :: forall l p i o id initialTriangleOsc
   . IsSymbol id
  => Parameters.InitialTriangleOsc initialTriangleOsc
  => CreateNode i id False o
  => Proxy id
  -> initialTriangleOsc
  -> Event (Core.TriangleOsc l p)
  -> GraphBuilder p i o (T.GraphUnit id T.TriangleOsc)
triangleOsc _ initialTriangleOsc attributes = GraphBuilder go
  where
  { frequency } = unwrap $ Parameters.toInitializeTriangleOsc initialTriangleOsc
  go i@(Core.AudioInterpret { makeTriangleOsc, setFrequency, setOnOff }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $
            makeTriangleOsc { id, parent: nothing, frequency, scope: "imperative" }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { frequency: Common.resolveAU i $ setFrequency <<< { id, frequency: _ }
            , onOff: bang <<< setOnOff <<< { id, onOff: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
