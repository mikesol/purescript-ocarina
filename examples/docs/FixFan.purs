module WAGS.Example.Docs.FixFan where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.FixFan.AI0 as AI0
import WAGS.Example.Docs.FixFan.AI1 as AI1
import WAGS.Example.Docs.FixFan.Fan0 as Fan0
import WAGS.Example.Docs.FixFan.Fan1 as Fan1
import WAGS.Example.Docs.FixFan.Fix0 as Fix0
import WAGS.Example.Docs.FixFan.Fix1 as Fix1
import WAGS.Example.Docs.FixFan.Intro as FFIntro
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import WAGS.Example.Docs.Util (ccassp, mkNext, scrollToTop)

data UIEvents = UIShown | ButtonClicked | SliderMoved Number
derive instance Eq UIEvents
px = Proxy :: Proxy
      """<div>
  <h1>Array, fan, and fix</h1>

  <h3>The anatomy of a Wags graph</h3>

  @intro@

  <h2>Arrays</h2>

  <p>To send several audio units through one, we use an <code>Array</code>.</p>

  @code0@

  <p>PureScript <code>Array</code>-s are extremely flexible and efficient, so go to town! For example, you can <code>map</code> (aka <code>&lt;#&gt;</code> when flipped) over a range of integers to create audio units, like in the example below.</p>

  @code1@

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes an audio signal and fans it out to multiple processing chains. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Fan takes two arguments:</p>

  <ul>
    <li>The node to fan out.</li>
    <li>A function that accepts a reference to this node and returns a new node that may or may not contain the input.</li>
  </ul>

  <p>Let's see an example below that fans one <code>playBuf</code> to five bandpass filters.</p>

  @code2@

  <p>Just for kicks, let's jack it up to forty bandpass filters.</p>

  @code3@

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts itself as an argument and returns... itself 🤯. You can use <code>fix</code> to create feedback loops!</p>

  @code4@

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Wags doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section 🎸</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>"""

fixFan :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent  -> Element lock payload
fixFan cca' dpage ssp ev = makePursx'  (Proxy :: _ "@") px
  { intro: nut (FFIntro.ffIntro cca' dpage ssp ev)
  , next: mnx AudioUnits
  , code0: nut $ AI0.ai0 ccb dpage ev
  , code1: nut $ AI1.ai1 ccb dpage ev
  , code2: nut $ Fan0.fan0 ccb dpage ev
  , code3: nut $ Fan1.fan1 ccb dpage ev
  , code4: nut $ Fix0.fix0 ccb dpage ev
  , code5: nut $ Fix1.fix1 ccb dpage ev
  }
  where
  mnx i = mkNext ev (dpage i *> scrollToTop)
  ccb = ccassp cca' ssp