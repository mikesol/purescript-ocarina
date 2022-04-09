module FRP.Event.Animate where

import Prelude

import Effect.Ref as Ref
import FRP.Event (Event, makeEvent)
import FRP.Event.Memoize (memoize)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

animationFrameEvent :: Event Unit
animationFrameEvent = memoize $ animationFrameEvent'

animationFrameEvent' :: Event Unit
animationFrameEvent' = makeEvent \k -> do
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
