module WAGS.Imperative.Create.PlayBuf where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common as Common
import WAGS.Common.Parameters.PlayBuf as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

playBuf
  :: forall l p i o id initialPlayBuf
   . IsSymbol id
  => Parameters.InitialPlayBuf initialPlayBuf
  => CreateNode i id False o
  => Proxy id
  -> initialPlayBuf
  -> Event (Core.PlayBuf l p)
  -> GraphBuilder p i o (T.GraphUnit id T.PlayBuf)
playBuf _ initialPlayBuf attributes = GraphBuilder go
  where
  { buffer, playbackRate, bufferOffset, duration } = unwrap $
    Parameters.toInitializePlayBuf initialPlayBuf
  go i@(Core.AudioInterpret { makePlayBuf, setBuffer, setOnOff, setDuration, setPlaybackRate, setBufferOffset }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makePlayBuf
            { id
            , parent: nothing
            , buffer
            , playbackRate
            , bufferOffset
            , duration
            , scope: "imperative"
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { buffer: bang <<< setBuffer <<< { id, buffer: _ }
            , playbackRate: Common.resolveAU i $ setPlaybackRate <<< { id, playbackRate: _ }
            , bufferOffset: bang <<< setBufferOffset <<< { id, bufferOffset: _ }
            , duration: bang <<< setDuration <<< { id, duration: _ }
            , onOff: bang <<< setOnOff <<< { id, onOff: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
