module WAGS.Imperative.Create.SinOsc where

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
import WAGS.Common.Parameters.SinOsc as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

sinOsc
  :: forall l p i o id initialSinOsc
   . IsSymbol id
  => Parameters.InitialSinOsc initialSinOsc
  => CreateNode i id False o
  => Proxy id
  -> initialSinOsc
  -> Event (Core.SinOsc l p)
  -> GraphBuilder p i o (T.GraphUnit id T.SinOsc)
sinOsc _ initialSinOsc attributes = GraphBuilder go
  where
  { frequency } = unwrap $ Parameters.toInitializeSinOsc initialSinOsc
  go di@(Core.AudioInterpret { makeSinOsc, setFrequency, setOnOff }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $
            makeSinOsc { id, parent: nothing, frequency, scope: "imperative" }
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
