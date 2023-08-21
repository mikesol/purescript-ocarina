module Ocarina.Clock where

import Prelude

import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal (new, read, write)
import Data.Foldable (traverse_)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Poll (Poll, poll)
import Ocarina.Core (AudioNumeric(..), AudioOnOff(..), _linear)
import Ocarina.Interpret (getAudioClockTime)
import Ocarina.WebAPI (AudioContext)
import Ocarina.WebAPI as WebAPI

type ACTime =
  { concreteTime :: Number, abstractTime :: Number, lookAhead :: Number }

type WriteHead (f :: Type -> Type) = f ACTime

withACTime :: forall a. AudioContext -> Event a -> Event { acTime :: Number, value :: a }
withACTime ctx e = makeEvent \k -> do
  subscribe e \value -> do
    acTime <- getAudioClockTime ctx
    k { acTime, value }

interval :: AudioContext -> Effect { fevent :: Event Number -> Event Number, unsubscribe :: Effect Unit }
interval ctx = do
  cref <- liftST $ new true
  iref <- liftST $ new Nothing
  vref <- liftST $ new Nothing
  pure
    { unsubscribe: liftST (write false cref) *> (liftST (read iref) >>= traverse_ clearTimeout)
    , fevent: \e -> makeEvent \k -> do
        subscribe e \newN -> do
          irr <- liftST $ read iref
          traverse_ clearTimeout irr
          cT' <- liftST $ read vref
          cT <- case cT' of
            Nothing -> getAudioClockTime ctx
            Just x -> pure x
          mkTimeout k (cT + newN) cref iref vref newN
    }
  where
  lookAhead = 0.04
  minVal = 0.01
  mkTimeout k n cref iref vref rt = do
    go0 <- liftST $ read cref
    when go0 do
      t <- getAudioClockTime ctx
      tid <- setTimeout (round ((max (n - t - lookAhead) minVal) * 1000.0)) do
        go1 <- liftST $ read cref
        when go1 do
          void $ liftST $ write (Just n) vref
          k n
          mkTimeout k (n + rt) cref iref vref rt
      void $ liftST $ write (Just tid) iref

writeHead :: Number -> WebAPI.AudioContext -> WriteHead Poll
writeHead lookAhead ctx = poll \eab -> makeEvent \k ->
  subscribe eab \ab -> do
    t <- getAudioClockTime ctx
    k (ab { concreteTime: t, abstractTime: t - lookAhead, lookAhead })

oo
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> AudioOnOff)
  -> f AudioOnOff
oo wh f = wh # map
  \{ concreteTime, abstractTime } ->
    let
      AudioOnOff { o, x } = f abstractTime
    in
      AudioOnOff { x, o: o + concreteTime }

fot
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> Number)
  -> f AudioNumeric
fot wh f = fot' wh (map (\n -> { o: 0.0, n: n }) f)

fot'
  :: forall f
   . Functor f
  => WriteHead f
  -> (Number -> { n :: Number, o :: Number })
  -> f AudioNumeric
fot' wh f = wh # map
  \{ concreteTime, abstractTime } ->
    let
      { n, o } = f abstractTime
    in
      AudioNumeric { t: _linear, o: o + concreteTime, n }
