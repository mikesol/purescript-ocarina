module WAGS.Example.KitchenSink.Types.SawtoothOsc where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Int (toNumber)
import Data.List ((..))
import Data.List as L
import Data.Tuple.Nested (type (/\))
import Math (cos, pi, pow, sin)
import WAGS.Change (ichange)
import WAGS.Create.Optionals (CSawtoothOsc, sawtoothOsc)
import WAGS.Example.KitchenSink.TLP.LoopSig (IxWAGSig')
import WAGS.Example.KitchenSink.Types.Empty (TopWith)
import WAGS.Graph.AudioUnit (TSawtoothOsc)
import WAGS.Graph.Parameter (modTime)

type SawtoothOscGraph
  = TopWith { sawtoothOsc :: Unit }
      ( sawtoothOsc :: TSawtoothOsc /\ {}
      )

ksSawtoothOscCreate :: { sawtoothOsc :: CSawtoothOsc }
ksSawtoothOscCreate = { sawtoothOsc: sawtoothOsc 440.0 }

stSpan :: L.List Number
stSpan = map (mul 0.04 <<< toNumber) (0 .. 200)

frontloadSawtoothOsc :: forall proof. IxWAGSig' SawtoothOscGraph SawtoothOscGraph proof Unit
frontloadSawtoothOsc = go stSpan
  where
  go :: L.List Number -> IxWAGSig' SawtoothOscGraph SawtoothOscGraph proof Unit
  go L.Nil = ipure unit

  go (L.Cons a b) = Ix.do
    deltaKsSawtoothOsc a
    go b

deltaKsSawtoothOsc :: forall proof. Number -> IxWAGSig' SawtoothOscGraph SawtoothOscGraph proof Unit
deltaKsSawtoothOsc time =
  let
    rad = pi * time
  in
    ichange
      { mix: 0.1 - 0.1 * (cos time)
      , sawtoothOsc: modTime (const time) $ pure (440.0 + 50.0 * ((sin (rad * 1.5)) `pow` 2.0))
      }
