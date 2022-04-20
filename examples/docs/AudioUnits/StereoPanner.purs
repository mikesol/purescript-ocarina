module WAGS.Example.Docs.AudioUnits.StereoPanner where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf, pan_)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (ctxAff, decodeAudioDataFromUri)
import WAGS.Parameter (bangOn)
import WAGS.Run (run2_)

px =
  Proxy    :: Proxy         """<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
"""

pan :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element Event payload
pan ccb _ ev = px ~~
  { pan: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/339/339822_5121236-lq.mp3")
          \buf -> run2_
            [pan_ 1.0 [loopBuf buf bangOn]]
      )
  }