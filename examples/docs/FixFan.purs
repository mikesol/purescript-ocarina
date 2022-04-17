module WAGS.Example.Docs.FixFan where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx')
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import WAGS.Example.Docs.Util (ccassp, mkNext, scrollToTop)

data UIEvents = UIShown | ButtonClicked | SliderMoved Number
derive instance Eq UIEvents
px = Proxy :: Proxy
      """<div>
  <h1>Fan, fix, and squiggles</h1>

  <h3>The anatomy of a Wags graph</h3>
  <p>
    In the <a @hwLink@ style="cursor:pointer;">hello world</a> section, we saw how to create and wire up two audio nodes: a <code>sinOsc</code>, or a sine-wave oscillator, is hooked up to a <code>gain</code> node. For some cases, feeding one audio node to another all the way up to a loudspeaker will be all you need. However, in most cases, you'll need to exploit three additional relationships:</p>
    <ul>
      <li><span style="font-weight:800px;">Many to one</span>, where many audio units pass through one.</li>
      <li><span style="font-weight:800px;">One to many</span>, where a single audio unit passes through many different ones.</li>
      <li><span style="font-weight:800px;">Feedback</span>, where an audio unit is an input to itself.</li>
    </ul>
    <p>This section will show how wags handles all three cases:</p>
    <ul>
      <li><span style="font-weight:800px;">~</span> or a squiggly or tilde, is the operation we'll use to send many audio units into one.</li>
      <li><span style="font-weight:800px;"><code>fan</code></span> is a function that we'll use to "fan" one audio node out to many.</li>
      <li><span style="font-weight:800px;"><code>fix</code></span> is the function we'll use to make an audio unit an input into itself.</li>
    </ul>
    <h2>The setup</h2>
    <p>
      To illustrate how <code>~</code>, <code>fan</code> and <code>fix</code> work, we're going to use three new audio units.
    </p>
      <ul>
        <li><code>delay</code>: A delay node</li>
        <li><code>bandpass</code>: A bandpass filter, meaning a filter that lets a single frequency band pass through.</li>
        <li><code>loopBuf</code>: Looped playback of a buffer. We'll use an MP3 file from freesound.org.</li>
      </ul>
  <h2>Squiggle, or tilde, or <code>~</code></h2>

  <p>To send several audio units through one, we use the tilde to chain the units together.</p>

  <pre><code></code></pre>

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes an audio signal and fans it out to multiple processing chains. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Fan takes two arguments:</p>

  <ul>
    <li>The node to fan out.</li>
    <li>A function that accepts a reference to this node and returns a new node that contains the input <i>at least</i> once.</li>
  </ul>

  <p>Let's see an example below that fans one <code>loopBuf</code> to five bandpass filters.</p>

  <pre><code></code></pre>

  <blockquote>The second argument to <code>fan</code> accepts a reference to the first node, <i>not</i> the node itself. To use this reference, you need to use the <code>input</code> function.</blockquote>

  <p>Just for kicks, let's jack it up to forty bandpass filters. Because we're using PureScript, we have the full power of its functional syntax to do our bidding.</p>

  <pre><code></code></pre>

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts as an argument of type <code>Input p c</code> representing itself and returns... itself 🤯. Like with <code>fan</code>, you'll need to use <code>input</code> to get an audio node out of the input. You use <code>fix</code> to create feedback!</p>

  <pre><code></code></pre>

  <blockquote>If you don't have some sort of delay line in your processing chain, either the WebAudio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Wags doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with squiggles, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>"""

fixFan :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> event SingleSubgraphEvent  -> Element event payload
fixFan cca' dpage ssp ev = makePursx'  (Proxy :: _ "@") px
  { hwLink: mnx HelloWorld
  , next: mnx AudioUnits
  }
  where
  mnx i = mkNext ev (dpage i *> scrollToTop)
  cca = ccassp cca' ssp