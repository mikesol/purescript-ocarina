module WAGS.Example.Docs.HelloWorld where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus)
import Deku.Attribute (cb, (:=))
import Deku.Control (text_)
import Deku.Core (Domable, envy)
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event.Class (bang, class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, sinOsc)
import WAGS.Core (bangOn)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent(..), SingleSubgraphPusher)
import WAGS.Example.Docs.Util (audioWrapper, ccassp, mkNext, scrollToTop)
import WAGS.Run (run2, run2_)

px =
  Proxy    :: Proxy         """<div>
  <h1>Hello world</h1>

  <h3>Wagging at 440Hz</h3>

  <p>Here's a "hello world" in Wags. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

  @code@
  @result@

  <h2>The <code>run</code> functions</h2>

  <p>The <code>run</code> family of functions run our audio and produces an unsubscribe function that we use to stop the audio. In this case, <code>run2_</code> does three extra things:
  <ul>
    <li>Wires up our session for two-channel audio. If the sources are mono, it will automatically scale them up to stereo.</li>
    <li>Automatically handles creation and destruction of audio contexts.</li>
    <li>Takes care of the subscription to the rendering engine.</li>
  </ul></p>

  <p>The <code>push</code> function comes from the <a href="https://github.com/mikesol/purescript-deku"><code>purescript-deku</code></a> framework and is used to register an unsubscribe effect. When we trigger the effect it in the <code>Just</code> branch of our pattern match, the audio turns off.</p>

  <h2>Audio units</h2>

  <p>The sound you hear when you play the example above is created with the statement <code>gain_ 0.15 [ sinOsc 440.0 bangOn ]</code>. The first function, <code>gain_</code>, creates a gain node with a volume of <code>0.15</code>. In WebAudio, gain ranges from <code>0.0</code> to <code>1.0</code> and can be converted to decibels using the following equation:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>In our case, a gain of <code>0.15</code> is roughly <code>-16.5 dB</code>.</p>

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>bangOn</code>, as the default is off for <i>all</i> sound generators in Wags. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore the anatomy of a Wags graph. Irrespective of the nodes comprising the graph, there are three basic concepts you need to be familiar with before you start diving into audio units: <a @next@ style="cursor:pointer;">array, fan, and fix</a>.</p>
</div>"""

helloWorld
  :: forall lock payload
   . CancelCurrentAudio
  -> (Page -> Effect Unit)
  -> SingleSubgraphPusher
  -> Event SingleSubgraphEvent
  -> Domable Effect lock payload
helloWorld cca' dpage ssp ev = makePursx' (Proxy :: _ "@") px
  { code: nut
      ( D.pre_
          [ D.code_
              [ text_
                  """case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push"""
              ]
          ]
      )
  , result: nut
      ( envy $ audioWrapper ev cca (\_ -> pure unit) $ \ctx _ -> run2 ctx [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
      )
  , next: mkNext ev cpage
  }
  where
  cpage = dpage FixFan *> scrollToTop
  cca = ccassp cca' ssp