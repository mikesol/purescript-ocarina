module Ocarina.Clock where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Foldable (traverse_)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (new, read, write)
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (Event, create, makeEvent, subscribe)
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

interval :: AudioContext -> Number -> Event Number -> Effect { event :: Event Number, unsubscribe :: Effect Unit }
interval ctx iN e = do
  { event, push } <- liftST create
  cref <- new true
  iref <- new Nothing
  acTime <- getAudioClockTime ctx
  vref <- new (acTime + iN)
  mkTimeout push iN cref iref vref iN
  unsub <- liftST $ subscribe e \newN -> do
    read iref >>= traverse_ clearTimeout
    cT <- read vref
    mkTimeout push (cT + newN) cref iref vref newN
  pure { event, unsubscribe: liftST unsub *> write false cref *> (read iref >>= traverse_ clearTimeout) }
  where
  lookAhead = 0.04
  minVal = 0.01
  mkTimeout k n cref iref vref rt = do
    go0 <- read cref
    when go0 do
      t <- getAudioClockTime ctx
      tid <- setTimeout (round ((max (n - t - lookAhead) minVal) * 1000.0)) do
        go1 <- read cref
        when go1 do
          write n vref
          k n
          mkTimeout k (n + rt) cref iref vref rt
      write (Just tid) iref

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
