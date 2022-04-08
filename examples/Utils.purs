module WAGS.Example.Utils where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, create, makeEvent, subscribe)
import WAGS.WebAPI (AudioContext)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

type ToCancel = { unsub :: Effect Unit, ctx :: AudioContext }
type RaiseCancellation = Maybe ToCancel -> Effect Unit

animationFrameEvent :: Effect (Event Unit /\ Effect Unit)
animationFrameEvent = do
  { push, event } <- create
  let
    e = makeEvent \k -> do
      w <- window
      running <- Ref.new true
      let
        fx = void $ flip requestAnimationFrame w do
          r' <- Ref.read running
          when r' do
            k unit
            fx
      fx
      pure $ Ref.write false running
  unsub <- subscribe e push
  pure (event /\ unsub)