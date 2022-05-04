module WAGS.Imperative.Create.DynamicsCompressor where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.DynamicsCompressor as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

dynamicsCompressor
  :: forall l p i o id initialDynamicsCompressor
   . IsSymbol id
  => Parameters.InitialDynamicsCompressor initialDynamicsCompressor
  => CreateNode i id False o
  => Proxy id
  -> initialDynamicsCompressor
  -> Event (Core.DynamicsCompressor l p)
  -> GraphBuilder p i o (T.GraphUnit id T.DynamicsCompressor)
dynamicsCompressor _ initialDynamicsCompressor attributes = GraphBuilder go
  where
  initializeDynamicsCompressor = unwrap $
    Parameters.toInitializeDynamicsCompressor initialDynamicsCompressor
  go i@(Core.AudioInterpret
          { makeDynamicsCompressor
          , setThreshold
          , setRatio
          , setKnee
          , setAttack
          , setRelease
          }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeDynamicsCompressor
            { id
            , parent: nothing
            , scope: "imperative"
            , threshold: initializeDynamicsCompressor.threshold
            , ratio: initializeDynamicsCompressor.ratio
            , knee: initializeDynamicsCompressor.knee
            , attack: initializeDynamicsCompressor.attack
            , release: initializeDynamicsCompressor.release
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { threshold: tmpResolveAU "imperative" i $ setThreshold <<< { id, threshold: _ }
            , ratio: tmpResolveAU "imperative" i $ setRatio <<< { id, ratio: _ }
            , knee: tmpResolveAU "imperative" i $ setKnee <<< { id, knee: _ }
            , attack: tmpResolveAU "imperative" i $ setAttack <<< { id, attack: _ }
            , release: tmpResolveAU "imperative" i $ setRelease <<< { id, release: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
