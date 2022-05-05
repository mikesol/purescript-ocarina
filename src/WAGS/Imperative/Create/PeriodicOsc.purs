module WAGS.Imperative.Create.PeriodicOsc where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common as Common
import WAGS.Common.Parameters.PeriodicOsc as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

periodicOsc
  :: forall l p i o id initialPeriodicOsc
   . IsSymbol id
  => Parameters.InitialPeriodicOsc initialPeriodicOsc
  => CreateNode i id False o
  => Proxy id
  -> initialPeriodicOsc
  -> Event (Core.PeriodicOsc l p)
  -> GraphBuilder p i o (T.GraphUnit id T.PeriodicOsc)
periodicOsc _ initialPeriodicOsc attributes = GraphBuilder go
  where
  { frequency, spec } = unwrap $ Parameters.toInitializePeriodicOsc initialPeriodicOsc
  go di@(Core.AudioInterpret { makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $
            makePeriodicOsc { id, parent: nothing, frequency, spec, scope: "imperative" }
          eventN = keepLatest
            ( attributes <#> unwrap >>> match
                { frequency: Common.resolveAU di (setFrequency <<< { id, frequency: _ })
                , onOff: bang <<< setOnOff <<< { id, onOff: _ }
                , spec: bang <<< setPeriodicOsc <<< { id, spec: _ }
                }
            )
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
