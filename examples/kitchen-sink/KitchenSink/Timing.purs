module WAGS.Example.KitchenSink.Timing where

import Prelude

import Control.Monad.State (State, evalState, get, modify)
import Data.Monoid.Additive (Additive)
import Data.Newtype (unwrap)
import Heterogeneous.Folding (hfoldl)
import Heterogeneous.Mapping (hmap)

type TimeInfo
  = { begin :: Additive Number, dur :: Additive Number, end :: Additive Number }

type TimeInfo'
  = { begin :: Number, dur :: Number, end :: Number }

type KitchenSinkTiming' a
  = { ksSinOsc :: a
    , ksTriangleOsc :: a
    , ksSquareOsc :: a
    , ksPeriodicOsc :: a
    , ksSawtoothOsc :: a
    , ksAllpass :: a
    , ksLowpass :: a
    , ksHighshelf :: a
    , ksLowshelf :: a
    , ksBandpass :: a
    , ksNotch :: a
    , ksPeaking :: a
    , ksHighpass :: a
    , ksMicrophone :: a
    , ksWaveShaper :: a
    , ksDelay :: a
    , ksFeedback :: a
    , ksLoopBuf :: a
    , ksStereoPanner :: a
    , ksConstant :: a
    , ksDynamicsCompressor :: a
    }

type Timed
  = State (Additive Number)

integrate :: Number -> Timed TimeInfo
integrate n =
  { begin: _, dur: _, end: _ }
    <$> get
    <*> (pure $ pure n)
    <*> (modify (append (pure n)))

deAdd :: TimeInfo -> TimeInfo'
deAdd { begin, dur, end } = { begin: unwrap begin, dur: unwrap dur, end: unwrap end }

timing :: KitchenSinkTiming' TimeInfo'
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
          , ksFeedback: _
          , ksLoopBuf: _
          , ksStereoPanner: _
          , ksConstant: _
          , ksDynamicsCompressor: _
          }
            <$> integrate 5.0 -- ksSinOsc
            <*> integrate 5.0 -- ksTriangleOsc
            <*> integrate 5.0 -- ksSquareOsc
            <*> integrate 5.0 -- ksPeriodicOsc
            <*> integrate 5.0 -- ksSawtoothOsc
            <*> integrate 10.0 -- ksAllpass
            <*> integrate 10.0 -- ksLowpass
            <*> integrate 10.0 -- ksHighshelf
            <*> integrate 10.0 -- ksLowshelf
            <*> integrate 10.0 -- ksBandpass
            <*> integrate 10.0 -- ksNotch
            <*> integrate 10.0 -- ksPeaking
            <*> integrate 10.0 -- ksHighpass
            <*> integrate 5.0 -- ksMicrophone
            <*> integrate 10.0 -- ksWaveShaper
            <*> integrate 10.0 -- ksDelay
            <*> integrate 10.0 -- ksFeedback
            <*> integrate 5.0 -- ksLoopBuf
            <*> integrate 5.0 -- ksStereoPanner
            <*> integrate 2.0 -- ksConstant
            <*> integrate 10.0 -- ksDynamicsCompressor
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
