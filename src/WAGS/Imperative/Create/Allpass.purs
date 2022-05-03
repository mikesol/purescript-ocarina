module WAGS.Imperative.Create.Allpass where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Allpass as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

allpass
  :: forall l p i o id initialAllpass
   . IsSymbol id
  => Parameters.InitialAllpass initialAllpass
  => CreateNode i id False o
  => Proxy id
  -> initialAllpass
  -> Event (Core.Allpass l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Allpass)
allpass _ initialAllpass attributes = GraphBuilder go
  where
  initializeAllpass = unwrap $ Parameters.toInitializeAllpass initialAllpass
  go i@(Core.AudioInterpret { makeAllpass, setFrequency, setQ }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $
            makeAllpass
              { id
              , parent: nothing
              , scope: "imperative"
              , frequency: initializeAllpass.frequency
              , q: initializeAllpass.q
              }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { frequency: tmpResolveAU "imperative" i (setFrequency <<< { id, frequency: _ })
            , q: tmpResolveAU "imperative" i (setQ <<< { id, q: _ })
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
