module WAGS.Example.KitchenSink.Timing where

import Prelude

import Control.Monad.State (State, evalState, get, modify)
import Data.Monoid.Additive (Additive)
import Data.Newtype (unwrap)
import Heterogeneous.Folding (hfoldl)
import Heterogeneous.Mapping (class HMap, hmap)

type TimeInfo
  = { begin :: Additive Number, dur :: Additive Number, end :: Additive Number }

type TimeInfo'
  = { begin :: Number, dur :: Number, end :: Number }

type KitchenSinkTiming
  = { ksSinOsc :: TimeInfo
    , ksTriangleOsc :: TimeInfo
    , ksSquareOsc :: TimeInfo
    , ksPeriodicOsc :: TimeInfo
    , ksSawtoothOsc :: TimeInfo
    , ksAllpass :: TimeInfo
    , ksLowpass :: TimeInfo
    , ksHighshelf :: TimeInfo
    , ksLowshelf :: TimeInfo
    , ksBandpass :: TimeInfo
    , ksNotch :: TimeInfo
    , ksPeaking :: TimeInfo
    , ksHighpass :: TimeInfo
    , ksMicrophone :: TimeInfo
    , ksWaveShaper :: TimeInfo
    , ksDelay :: TimeInfo
    , ksDynamicsCompressor :: TimeInfo
    }

type Timed
  = State (Additive Number)

integrate :: Number -> Timed TimeInfo
integrate n =
  { begin: _, dur: _, end: _ }
    <$> get
    <*> (pure $ pure n)
    <*> (modify (append (pure n)))

data KSFold
  = KSFold

deAdd :: TimeInfo -> TimeInfo'
deAdd { begin, dur, end } = { begin: unwrap begin, dur: unwrap dur, end: unwrap end }

timing :: forall a. HMap (TimeInfo -> TimeInfo') KitchenSinkTiming a => a
timing =
  hmap deAdd
    $ evalState
        ( { ksSinOsc: _
          , ksTriangleOsc: _
          , ksSquareOsc: _
          , ksPeriodicOsc: _
          , ksSawtoothOsc: _
          , ksAllpass: _
          , ksLowpass: _
          , ksHighshelf: _
          , ksLowshelf: _
          , ksBandpass: _
          , ksNotch: _
          , ksPeaking: _
          , ksHighpass: _
          , ksMicrophone: _
          , ksWaveShaper: _
          , ksDelay: _
          , ksDynamicsCompressor: _
          }
            <$> integrate 5.0
            <*> integrate 5.0
            <*> integrate 5.0
            <*> integrate 5.0
            <*> integrate 5.0
            <*> integrate 10.0
            <*> integrate 10.0
            <*> integrate 10.0
            <*> integrate 10.0
            <*> integrate 10.0
            <*> integrate 10.0
            <*> integrate 10.0
            <*> integrate 10.0
            <*> integrate 5.0
            <*> integrate 10.0
            <*> integrate 10.0
            <*> integrate 10.0
        )
        mempty

pieceTime :: Number
pieceTime = hfoldl (\(a :: Number) (b :: TimeInfo') -> max a b.end) 0.0 timing

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b
