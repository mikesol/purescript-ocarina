module WAGS.Imperative.Create.StereoPanner where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.StereoPanner as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

pan
  :: forall l p i o id initialStereoPanner
   . IsSymbol id
  => Parameters.InitialStereoPanner initialStereoPanner
  => CreateNode i id False o
  => Proxy id
  -> initialStereoPanner
  -> Event (Core.StereoPanner l p)
  -> GraphBuilder p i o (T.GraphUnit id T.StereoPanner)
pan _ initialStereoPanner attributes = GraphBuilder go
  where
  initializeStereoPanner = unwrap
    $ Parameters.toInitializeStereoPanner initialStereoPanner
  go i@(Core.AudioInterpret { makeStereoPanner, setPan }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeStereoPanner
            { id
            , parent: nothing
            , scope: "imperative"
            , pan: initializeStereoPanner.pan
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { pan: tmpResolveAU "imperative" i $ setPan <<< { id, pan: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
