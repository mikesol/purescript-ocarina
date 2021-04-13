module WAGS.Example.HelloWorld where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import FRP.Event (subscribe)
import Math (pi, sin)
import WAGS (FFIAudio(..), Frame0, Scene, SceneI, FFIAudio', change, create, env, gain, loop, run, sinOsc, speaker, start, (@>))
import WAGS.Control.Qualified as Ix

scene time =
  let
    rad = pi * time
  in
    speaker
      $ ( (gain 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
            /\ (gain 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
            /\ (gain 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
            /\ (gain 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
            /\ unit
        )

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
piece =
  Ix.do
    start
    { time } <- env
    create (scene time) $> Right unit
    @> loop
        ( const
            $ Ix.do
                { time } <- env
                ivoid $ change (scene time)
        )

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm = let x initialTime = mkCofree initialTime \_ -> x initialTime in x 20

myRun :: FFIAudio' -> Effect (Effect Unit)
myRun ffiAudio =
  subscribe
    (run { easingAlgorithm } (FFIAudio ffiAudio) (pure unit) (pure unit) piece)
    (const $ pure unit)

main :: Effect Unit
main = pure unit