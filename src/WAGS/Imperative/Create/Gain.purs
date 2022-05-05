module WAGS.Imperative.Create.Gain where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Gain as Parameters
import WAGS.Core as Core
import WAGS.Common as Common
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

-- | Creates a `Gain` node.
-- |
-- | ```purescript
-- | gain <- Create.gain (Proxy :: _ "gain") 1.0 empty
-- | ```
gain
  :: forall l p i o id initialGain
   . IsSymbol id
  => Parameters.InitialGain initialGain
  => CreateNode i id False o
  => Proxy id
  -> initialGain
  -> Event (Core.Gain l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Gain)
gain _ initialGain attributes = GraphBuilder go
  where
  { gain } = unwrap $ Parameters.toInitializeGain initialGain
  go i@(Core.AudioInterpret { makeGain, setGain }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $
            makeGain
              { id
              , parent: nothing
              , gain
              , scope: "imperative"
              }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { gain: Common.resolveAU i $ setGain <<< { id, gain: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
