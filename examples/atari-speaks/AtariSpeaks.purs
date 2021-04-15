module WAGS.Example.AtariSpeaks where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (Promise)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import FRP.Event (subscribe)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS (AudioContext, BrowserAudioBuffer, FFIAudio(..), FFIAudio', Frame0, Gain, GetSetAP, LoopBuf, Scene, SceneI, Speaker, change, create, decodeAudioDataFromUri, env, gain, loop, loopBuf, run, speaker, start, (@>))
import WAGS.Control.Qualified as Ix

vol = 1.4 :: Number

scene ::
  Number ->
  Speaker
    ( (Gain GetSetAP (LoopBuf "atar" GetSetAP))
        /\ (Gain GetSetAP (LoopBuf "atar" GetSetAP))
        /\ (Gain GetSetAP (LoopBuf "atar" GetSetAP))
        /\ Unit
    )
scene time =
  let
    rad = pi * time
  in
    speaker
      $ ( ( gain (0.3 * vol)
              ( loopBuf (0.0 /\ 0.0)
                  (Proxy :: _ "atar")
                  (1.0 + 0.1 * sin rad)
              )
          )
            /\ ( gain (0.15 * vol)
                  ( loopBuf
                      ( (0.1 + 0.1 * sin rad)
                          /\ (0.5 + 0.25 * sin (2.0 * rad))
                      )
                      (Proxy :: _ "atar")
                      (1.5 + 0.1 * sin (2.0 * rad))
                  )
              )
            /\ ( gain (0.3 * vol)
                  (loopBuf (0.0 /\ 0.0) (Proxy :: _ "atar") 0.25)
              )
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
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

atari :: AudioContext -> Effect (Promise BrowserAudioBuffer)
atari ctx = decodeAudioDataFromUri ctx "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"

myRun :: FFIAudio' -> Effect (Effect Unit)
myRun ffiAudio =
  subscribe
    (run (pure unit) (pure unit) { easingAlgorithm } (FFIAudio ffiAudio) piece)
    (const $ pure unit)

main :: Effect Unit
main = pure unit
