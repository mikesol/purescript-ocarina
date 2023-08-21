module Ocarina.Run where

import Prelude

import Bolson.Core as B
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as Ref
import Control.Plus (empty)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Effect (Effect)
import FRP.Event (create, subscribe)
import FRP.Poll (Poll, sample)
import Ocarina.Control (speaker2)
import Ocarina.Core as C
import Ocarina.Interpret (FFIAudioSnapshot, close, context, effectfulAudioInterpret, makeFFIAudioSnapshot)
import Ocarina.WebAPI (AudioContext)

run2_
  :: (Array (C.Audible D2 (FFIAudioSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run2_ s = do
  ctx <- context
  map (_ *> close ctx) (run2 ctx s)

run2
  :: AudioContext
  -> (Array (C.Audible D2 (FFIAudioSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run2 ctx s = do
    ffi <- makeFFIAudioSnapshot ctx
    rf0 <- liftST $ Ref.new 0
    rf1 <- liftST $ Ref.new Map.empty
    let ee f = f ffi
    ep <- liftST $ create
    u <- liftST $ subscribe (sample (speaker2 s (effectfulAudioInterpret rf0 rf1 ee)) ep.event) ee
    ep.push identity
    pure $ liftST u
run2e_
  :: (Poll (Array (C.Audible D2 (FFIAudioSnapshot -> Effect Unit))))
  -> Effect (Effect Unit)
run2e_ s = do
  ctx <- context
  map (_ *> close ctx) (run2e ctx s)

run2e
  :: AudioContext
  -> (Poll (Array (C.Audible D2 (FFIAudioSnapshot -> Effect Unit))))
  -> Effect (Effect Unit)
run2e ctx s = do
    ffi <- makeFFIAudioSnapshot ctx
    rf0 <- liftST $ Ref.new 0
    rf1 <- liftST $ Ref.new Map.empty
    let ee f = f ffi
    ep <- liftST $ create
    u <- liftST $ subscribe (sample (speaker2 [B.DynamicChildren' $ B.DynamicChildren (map (Tuple empty <<< B.FixedChildren' <<< B.FixedChildren) s)] (effectfulAudioInterpret rf0 rf1 ee)) ep.event) ee
    ep.push identity
    pure $ liftST u