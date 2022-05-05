module WAGS.Imperative.Create.Analyser where

import Prelude

import Control.Alternative ((<|>))
import Data.Int (pow)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Analyser as Parameters
import WAGS.Core (ChannelCountMode(..), ChannelInterpretation(..), Po2(..))
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

analyser
  :: forall p i o id initialAnalyser
   . IsSymbol id
  => Parameters.InitialAnalyser initialAnalyser
  => CreateNode i id False o
  => Proxy id
  -> initialAnalyser
  -> Event Core.Analyser
  -> GraphBuilder p i o (T.GraphUnit id T.Analyser)
analyser _ initialAnalyser attributes = GraphBuilder go
  where
  initializeAnalyser = unwrap $ Parameters.toInitializeAnalyser initialAnalyser
  go (Core.AudioInterpret { makeAnalyser, setAnalyserNodeCb }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $
            makeAnalyser
              { id
              , parent: nothing
              , scope: "imperative"
              , cb: initializeAnalyser.cb
              , fftSize: pow 2 $ case initializeAnalyser.fftSize of
                  TTT7 -> 7
                  TTT8 -> 8
                  TTT9 -> 9
                  TTT10 -> 10
                  TTT11 -> 11
                  TTT12 -> 12
                  TTT13 -> 13
              , maxDecibels: initializeAnalyser.maxDecibels
              , minDecibels: initializeAnalyser.minDecibels
              , smoothingTimeConstant: initializeAnalyser.smoothingTimeConstant
              , channelCount: initializeAnalyser.channelCount
              , channelCountMode: case initializeAnalyser.channelCountMode of
                  Explicit -> "explicit"
                  Max -> "max"
                  ClampedMax -> "clamped-max"
              , channelInterpretation: case initializeAnalyser.channelInterpretation of
                  Speakers -> "speakers"
                  Discrete -> "discrete"
              }
          eventN = attributes <#> unwrap >>> match
            { cb: setAnalyserNodeCb <<< { id, cb: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
