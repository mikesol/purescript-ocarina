module WAGS.Example.Docs.FixFan where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (class IsEvent)
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
  <h1>Fan, fix, and squiggles</h1>

  <h3>The anatomy of a Wags graph</h3>

  @intro@

  <h2>Squiggle, or tilde, or <code>~</code></h2>

  <p>To send several audio units through one, we use the tilde to chain the units together.</p>

  @code0@

  <p>Sometimes, instead of chaining units together, we want to work with higher-level abstractions. In this case, we can work directly with a <code>NonEmptyArray</code> and use the <code>ai</code> function to make the array consumable by other audio units.</p>

  @code1@

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes an audio signal and fans it out to multiple processing chains. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Fan takes two arguments:</p>

  <ul>
    <li>The node to fan out.</li>
    <li>A function that accepts a reference to this node and returns a new node that contains the input <i>at least</i> once.</li>
  </ul>

  <p>Let's see an example below that fans one <code>playBuf</code> to five bandpass filters.</p>

  @code2@

  <blockquote>The second argument to <code>fan</code> accepts a reference to the first node, <i>not</i> the node itself. To use this reference, you need to use the <code>input</code> function.</blockquote>

  <p>Just for kicks, let's jack it up to forty bandpass filters.</p>

  @code3@

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts as an argument of type <code>Input p c</code> representing itself and returns... itself 🤯. Like with <code>fan</code>, you'll need to use <code>input</code> to get an audio node out of the input. You use <code>fix</code> to create feedback!</p>

  @code4@

  <blockquote>If you don't have some sort of delay line in your processing chain, either the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Wags doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting fixes to create a mega-feedback loop! When doing so, you'll likely have nested fixes. In this case, you'll need to use the <code>hint</code> function along with the input to the outer loop. <code>hint</code> helps the compiler with type inference: otherwise, it can't infer the type of the fixed point because of an otherwise-intractable situation where two fixed points' types depend on each other.</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section 🎸</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with squiggles, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>"""

fixFan :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> event SingleSubgraphEvent  -> Element event payload
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