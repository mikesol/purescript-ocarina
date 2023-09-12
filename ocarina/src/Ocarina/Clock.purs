module Ocarina.Clock where

import Prelude

import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal (new, read, write)
import Data.Foldable (traverse_)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Op (Op(..))
import Effect (Effect)
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (Event, makeEventE, subscribe)
import Ocarina.Core (AudioNumeric(..), AudioOnOff(..), _linear)
import Ocarina.Interpret (getAudioClockTime)
import Ocarina.WebAPI (AudioContext)
import Ocarina.WebAPI as WebAPI
import Safe.Coerce (coerce)

type ACTime =
  { concreteTime :: Number, abstractTime :: Number, lookAhead :: Number }

type WriteHead (f :: Type -> Type) = f ACTime

withACTime :: forall a. AudioContext -> Op (Effect Unit) { acTime :: Number, value :: a } -> Op (Effect Unit) a
withACTime ctx = (coerce :: (_ -> a -> _ Unit) -> _ -> _) go
  where
  go f value = do
    t <- getAudioClockTime ctx
    f { value, acTime: t }

interval'
  :: forall a. (Op (Effect Unit) a -> Op (Effect Unit) Number) -> AudioContext -> Event Number -> Effect { event :: Event a, unsubscribe :: Effect Unit }
interval' f ctx e = makeEventE \k -> do
  cref <- liftST $ new true
  iref <- liftST $ new Nothing
  vref <- liftST $ new Nothing
  unsubscribe <- subscribe e \newN -> do
    irr <- liftST $ read iref
    traverse_ clearTimeout irr
    cT' <- liftST $ read vref
    cT <- case cT' of
      Nothing -> getAudioClockTime ctx
      Just x -> pure x
    mkTimeout ((coerce :: _ -> _ -> _ -> _ Unit) f k) (cT + newN) cref iref vref newN
  pure do
    liftST (write false cref) *> unsubscribe *> (liftST (read iref) >>= traverse_ clearTimeout)
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

interval ∷ AudioContext → Event Number → Effect { event ∷ Event Number, unsubscribe ∷ Effect Unit }
interval = interval' identity

withWriteHead :: forall a. Number -> WebAPI.AudioContext -> Op (Effect Unit) { value :: a, ac :: ACTime } -> Op (Effect Unit) a
withWriteHead lookAhead ctx = (coerce :: (_ -> a -> _ Unit) -> _ -> _) go
  where
  go f value = do
    t <- getAudioClockTime ctx
    f { value, ac: { concreteTime: t, abstractTime: t - lookAhead, lookAhead } }

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
