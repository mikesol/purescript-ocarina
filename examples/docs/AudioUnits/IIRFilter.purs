module WAGS.Example.Docs.AudioUnits.IIRFilter where

import Prelude

import Control.Plus (class Plus)
import Data.Tuple.Nested ((/\))
import Data.Vec (empty, (+>))
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (iirFilter, loopBuf)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px =
  Proxy    :: Proxy         """<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Wags enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\{loop, verb} -> run2_
            $ iirFilter
                ( (0.00020298 +> 0.0004059599 +> 0.00020298 +> empty)
                    /\ (1.0126964558 +> -1.9991880801 +> 0.9873035442 +> empty)
                )
            $ loopBuf buf pureOn</code></pre>
  ~iirFilterEx~
  </section>
"""

iirFilterEx
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
iirFilterEx ccb _ ev = px ~~
  { iirFilterEx: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
          \buf -> run2_
            $ iirFilter
                ( (0.00020298 +> 0.0004059599 +> 0.00020298 +> empty)
                    /\ (1.0126964558 +> -1.9991880801 +> 0.9873035442 +> empty)
                )
            $ loopBuf buf pureOn
      )
  }