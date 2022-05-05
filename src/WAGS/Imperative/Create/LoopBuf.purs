module WAGS.Imperative.Create.LoopBuf where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.LoopBuf as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

loopBuf
  :: forall l p i o id initialLoopBuf
   . IsSymbol id
  => Parameters.InitialLoopBuf initialLoopBuf
  => CreateNode i id False o
  => Proxy id
  -> initialLoopBuf
  -> Event (Core.LoopBuf l p)
  -> GraphBuilder p i o (T.GraphUnit id T.LoopBuf)
loopBuf _ initialLoopBuf attributes = GraphBuilder go
  where
  { buffer, playbackRate, loopStart, loopEnd, duration } = unwrap $
    Parameters.toInitializeLoopBuf initialLoopBuf
  go i@(Core.AudioInterpret { makeLoopBuf, setBuffer, setOnOff, setPlaybackRate, setLoopStart, setLoopEnd }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeLoopBuf
            { id
            , parent: nothing
            , buffer
            , playbackRate
            , loopStart
            , loopEnd
            , duration
            , scope: "imperative"
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { buffer: bang <<< setBuffer <<< { id, buffer: _ }
            , playbackRate: tmpResolveAU "imperative" i $ setPlaybackRate <<< { id, playbackRate: _ }
            , loopStart: bang <<< setLoopStart <<< { id, loopStart: _ }
            , loopEnd: bang <<< setLoopEnd <<< { id, loopEnd: _ }
            , onOff: bang <<< setOnOff <<< { id, onOff: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
