module Ocarina.Example.Docs.MultiChannel where

import Prelude

import Control.Plus (class Plus)
import Deku.Attribute (cb, (:=))
import Deku.Core (Domable)
import Deku.DOM as D
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import Ocarina.Example.Docs.Util (scrollToTop)

px = Proxy :: Proxy
      """<div>
  <h1>Merge and split</h1>

  <h3>Inputs and outputs abound!</h3>
  <p>
    Web audio allows you to merge and split arbitrary audio. This is essential when you're working with complex audio setups like 5.1 surround sound or novel headphones used in some gaming setups. Ocarina allows you to both split and merge arbitrary signals using Web Audio's native merger and splitter nodes.
  </p>

  <h2>Merging</h2>

  <p>Merging audio in ocarina looks like any node that takes multiple inputs, but instead of accepting something of type <code>AudioInput</code>, it accepts a <i>vector of audio inputs</i>.</p>

  <h2>Splitting</h2>

  <p>Splitting is the inverse operation of merging: it takes a single audio node and splits it into its separate channels. In doing so, it resembles <code>fan</code>, but instead of fanning the audio, it splits it into mono-channel audio.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to merge and split audio. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>"""

multiChannel :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent  -> Domable Effect lock payload
multiChannel _ dpage _ _ = px ~~
  { next: bang (D.OnClick := (cb (const $ dpage AudioWorklets *> scrollToTop)))
  }