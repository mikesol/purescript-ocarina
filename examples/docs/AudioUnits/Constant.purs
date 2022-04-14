module WAGS.Example.Docs.AudioUnits.Constant where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, sinOsc)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

px = Proxy :: Proxy """<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Wags uses this feature of constant nodes under the hood to optimize certain computations.</p>

  ~constant~
  </section>
"""

constant :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> Element event payload
constant ccb _ = px ~~
  { constant: nut
      ( audioWrapper ccb (pure unit) \_ -> run2_ [ gain_ 0.05 [ sinOsc 440.0 pureOn ] ]
      )
  }