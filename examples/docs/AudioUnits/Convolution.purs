module WAGS.Example.Docs.AudioUnits.Convolution where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (convolver, loopBuf)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px = Proxy :: Proxy """<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\{loop, verb} -> run2_
  $ convolver verb
  $ loopBuf loop pureOn</code></pre>

  ~convolution~
  </section>
"""

convolution :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
convolution ccb _ ev = px ~~
  { convolution: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> { loop: _, verb: _ }
         <$> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3" <*> decodeAudioDataFromUri ctx "https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a")
          \{loop, verb} -> run2_
            $ convolver verb $ loopBuf loop pureOn
      )
  }