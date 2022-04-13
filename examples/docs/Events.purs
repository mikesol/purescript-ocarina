module WAGS.Example.Docs.Events where

import Prelude

import Control.Plus (class Plus)
import Deku.Attribute (cb, (:=))
import Deku.Core (Element)
import Deku.DOM as D
import WAGS.Example.Docs.Types (Page(..))
import WAGS.Example.Docs.Util (scrollToTop)
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))

data UIEvents = UIShown | ButtonClicked | SliderMoved Number
derive instance Eq UIEvents
px = Proxy :: Proxy
      """<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Wags.
  </p>

  <h2>Events, a primer</h2>

  <h2>Example 1: A slider</h2>

  <h2>Example 2: A counter</h2>

  <h2>Example 3: requestAnimationFrame</h2>

  <h2>Example 4: Composing events</h2>

  <h2>Next steps</h2>
  <p>In this section, saw how to react to events. In the next section, we'll build more complex graphs using a pair of functions called <a ~next~ style="cursor:pointer;"><code>fix</code> and <code>fan</code></a>.</p>
</div>"""

events :: forall event payload. IsEvent event => Plus event => (Page -> Effect Unit) -> Element event payload
events dpage = px ~~
  { next: bang (D.OnClick := (cb (const $ dpage FixFan *> scrollToTop)))
  }