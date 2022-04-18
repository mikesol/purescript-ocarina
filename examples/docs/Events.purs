module WAGS.Example.Docs.Events where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.Events.Ex0 as Ex0
import WAGS.Example.Docs.Events.Ex1 as Ex1
import WAGS.Example.Docs.Events.Ex2 as Ex2
import WAGS.Example.Docs.Events.Primer as Primer
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import WAGS.Example.Docs.Util (ccassp, mkNext, scrollToTop)

data UIEvents = UIShown | ButtonClicked | SliderMoved Number
derive instance Eq UIEvents
px = Proxy :: Proxy
      """<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Wags.
  </p>

  @primer@
  <h2>Events in Wags</h2>
  <p>Wags follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. Let's see a few examples:</p>

  <p>In practice, you'll never need to use the raw format presented above. The <code>WAGS.Properties</code> module has smart constructors for all these values. Below is a table showing the constructor and the units to which it applies.</p>

  <p>You can use this consistent pattern to transform any <code>Event</code> into something that Wags can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Wags:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Wags. Idiomatic Wags projects use functional reactive programming as a way to "steer" web audio, and Wags aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>

  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. In the next section, we'll look at how to <a @next@ style="cursor:pointer;">merge and split audio</a>.</p>
</div>"""

events :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent -> Element Event payload
events cca' dpage ssp ev = makePursx'  (Proxy :: _ "@") px
  { next: mnx MultiChannel
  , primer: nut $ Primer.primer
  , ex0: nut $ Ex0.ex0 ccb dpage ev
  , ex1: nut $ Ex1.ex1 ccb dpage ev
  , ex2: nut $ Ex2.ex2 ccb dpage ev
  }
  where
  mnx i = mkNext ev (dpage i *> scrollToTop)
  ccb = ccassp cca' ssp