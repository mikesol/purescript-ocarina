module WAGS.Imperative.Create.SawtoothOsc where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common as Common
import WAGS.Common.Parameters.SawtoothOsc as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

sawtoothOsc
  :: forall l p i o id initialSawtoothOsc
   . IsSymbol id
  => Parameters.InitialSawtoothOsc initialSawtoothOsc
  => CreateNode i id False o
  => Proxy id
  -> initialSawtoothOsc
  -> Event (Core.SawtoothOsc l p)
  -> GraphBuilder p i o (T.GraphUnit id T.SawtoothOsc)
sawtoothOsc _ initialSawtoothOsc attributes = GraphBuilder go
  where
  { frequency } = unwrap $ Parameters.toInitializeSawtoothOsc initialSawtoothOsc
  go di@(Core.AudioInterpret { makeSawtoothOsc, setFrequency, setOnOff }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $
            makeSawtoothOsc { id, parent: nothing, frequency, scope: "imperative" }
          eventN = keepLatest
            ( attributes <#> unwrap >>> match
                { frequency: Common.resolveAU di (setFrequency <<< { id, frequency: _ })
                , onOff: bang <<< setOnOff <<< { id, onOff: _ }
                }
            )
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
