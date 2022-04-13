module WAGS.Example.Docs.FixFan where

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
  <h1>Fan and fix</h1>

  <h3>Glorious graphs for fun and profit</h3>
  <p>
    Up until now, we've been treating audio sort of like one would treat a webpage. There are elements (like an oscillator) inside other elements (like a gain node) that dutifully trickle up to a terminal node, like a speaker or recorder. But this overlooks two crucial features of audio:
  </p>
  <ul>
    <li>The ability for a signal to be processed via multiple pathways.</li>
    <li>Feedback loops.</li>
  </ul>
  <p>This section fixes that using a pair of functions: <code>fan</code> to fan out a node to multiple processing chains, and <code>fix</code> to create feedback.</p>

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes an audio signal and fans it out to multiple processing chains. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Note that fan passes an argument of type <code>Input p c</code> to its callback, so you can't use it as a node. Instead, you'll need to use the <code>input</code> function to get a node out of it.</p>

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts as an argument of type <code>Input p c</code> representing itself and returns... itself 🤯. Like with <code>fan</code>, you'll need to use <code>input</code> to get an audio node out of the input. You use <code>fix</code> to create feedback!</p>

  <h2>Gain revisited</h2>

  <p>Up until now, we've been using gain nodes in a relatively innocuous way. But now that we're in the land of <code>fix</code> and <code>fan</code>, certain complications arise. Specifically, if you're mixing two different input sources, or if you're mixing an input source with a generator, you'll need to use a different flavor of gain called <code>gainx</code>.</p>

  <p><code>gainx</code>, like <code>gain</code>, accepts a bunch of incoming audio units, but the difference is that gainx accumulates its children's scopes. So what the heck is a scope? Every time you use <code>fan</code> or <code>fix</code>, a new scope is created for each fanned or fixed audio unit. These scopes are hermetic. Without scopes, we could (for example) sneak an audio unit in a callback (ie the analyser or recorder callback), pass it to a top-level DOM component, and then spread it all over the place. At this point, the audio unit has gone rogue. It may or may not have a viable context, may or may not have been deleted, etc. And no one likes rogue audio units.</p>

  <p>Scopes solve this problem, but because different <code>Input</code>-s have different scopes, you need a safe way to blend them together. Enter <code>gainx</code>.</p>

  <p>Here's an example of how <code>gainx</code> can mix an match units from the same scope.</p>

  <p>Without <code>gainx</code>, you'd get a compile error. Go on, try to compile the code below, I dare you!</p>

  <p>Ok, ok, just kidding, I wouldn't wish a compile error on you. I've bitten the bullet and done this one for you. Here's what it looks like.</p>

  <p>Yikes! Who wants that? To avoid it, use <code>gainx</code> when working with <code>fan</code> and <code>fix</code>.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to fan one audio node to many processing chains and how to create a fixed point, aka feedback, for a node. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>"""

fixFan :: forall event payload. IsEvent event => Plus event => (Page -> Effect Unit) -> Element event payload
fixFan dpage = px ~~
  { next: bang (D.OnClick := (cb (const $ dpage AudioWorklets *> scrollToTop)))
  }