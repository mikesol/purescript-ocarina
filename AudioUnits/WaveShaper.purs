module Ocarina.Example.Docs.AudioUnits.WaveShaper where

import Prelude

import Data.Array ((..))
import Data.Int (toNumber)
import Data.Number (pi)
import Data.Ord (abs)
import Deku.Control (text_)
import Deku.Core (Domable)
import Bolson.Core (envy)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import Ocarina.Control (loopBuf, waveShaper)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Interpret (decodeAudioDataFromUri, makeFloatArray)
import Ocarina.Run (run2)

px =
  Proxy :: Proxy """<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
"""

waveShaperEx
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
waveShaperEx ccb _ ev = px ~~
  { code: nut
      ( text_
          """do
  let
    makeDistortionCurve :: Number -> Array Number
    makeDistortionCurve k =
      map
        ( \i ->
            let
              x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
            in
              (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
        )
        (0 .. (n_samples - 1))
      where
      n_samples = 44100

      deg = pi / 180.0
  run2_
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]"""
      )
  , waveShaper: nut
      ( envy $ audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/339/339822_5121236-lq.mp3")
          \ctx buf -> do
            let
              makeDistortionCurve :: Number -> Array Number
              makeDistortionCurve k =
                map
                  ( \i ->
                      let
                        x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
                      in
                        (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
                  )
                  (0 .. (n_samples - 1))
                where
                n_samples = 44100

                deg = pi / 180.0
            run2 ctx
              [waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [loopBuf buf bangOn]]
      )
  }