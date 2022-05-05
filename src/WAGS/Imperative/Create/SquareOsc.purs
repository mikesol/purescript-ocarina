module WAGS.Imperative.Create.SquareOsc where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common as Common
import WAGS.Common.Parameters.SquareOsc as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

squareOsc
  :: forall l p i o id initialSquareOsc
   . IsSymbol id
  => Parameters.InitialSquareOsc initialSquareOsc
  => CreateNode i id False o
  => Proxy id
  -> initialSquareOsc
  -> Event (Core.SquareOsc l p)
  -> GraphBuilder p i o (T.GraphUnit id T.SquareOsc)
squareOsc _ initialSquareOsc attributes = GraphBuilder go
  where
  { frequency } = unwrap $ Parameters.toInitializeSquareOsc initialSquareOsc
  go di@(Core.AudioInterpret { makeSquareOsc, setFrequency, setOnOff }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $
            makeSquareOsc { id, parent: nothing, frequency, scope: "imperative" }
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
