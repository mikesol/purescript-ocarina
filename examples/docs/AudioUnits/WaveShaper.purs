module WAGS.Example.Docs.AudioUnits.WaveShaper where

import Prelude

import Control.Plus (class Plus)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Ord (abs)
import Deku.Control (text_)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import Math (pi)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf, waveShaper)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri, makeFloatArray)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px =
  Proxy :: Proxy """<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
"""

waveShaperEx
  :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
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
  wicked <- makeFloatArray (makeDistortionCurve 400.0)
  run2_
    $ waveShaper wicked $ loopBuf buf pureOn"""
      )
  , waveShaper: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/339/339822_5121236-lq.mp3")
          \buf -> do
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
            wicked <- makeFloatArray (makeDistortionCurve 400.0)
            run2_
              $ waveShaper wicked $ loopBuf buf pureOn
      )
  }