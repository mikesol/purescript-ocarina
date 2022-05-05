module WAGS.Imperative.Create.WaveShaper where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant.Maybe (nothing)
import FRP.Event (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.WaveShaper as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

waveShaper
  :: forall p i o id initialWaveShaper
   . IsSymbol id
  => Parameters.InitialWaveShaper initialWaveShaper
  => CreateNode i id False o
  => Proxy id
  -> initialWaveShaper
  -> GraphBuilder p i o (T.GraphUnit id T.WaveShaper)
waveShaper _ initialWaveShaper = GraphBuilder go
  where
  { curve, oversample } = unwrap $ Parameters.toInitializeWaveShaper initialWaveShaper
  go (Core.AudioInterpret { makeWaveShaper }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
        in
          bang $ makeWaveShaper
            { id
            , parent: nothing
            , scope: "imperative"
            , curve
            , oversample
            }
    , result: T.GraphUnit
    }
