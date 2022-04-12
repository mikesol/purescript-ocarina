module WAGS.Example.Docs.HelloWorld where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus)
import Data.Compactable (compact)
import Data.Either (hush)
import Data.Exists (mkExists)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event.Class (bang, class IsEvent)
import Run (run2_)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, sinOsc)
import WAGS.Example.Docs.Types (Page(..))
import WAGS.Example.Docs.Util (scrollToTop)
import WAGS.Parameter (pureOn)

px = Proxy :: Proxy """<div>
  <h1>Hello world</h1>

  <h3>Making A440</h3>

  <p>Here is "hello world" in Wags. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

  ~code~

  <p>And here's what it produces:</p>

  <blockquote> ~result~ </blockquote>

  <p>You gotta start somewhere!</p>

  <h2>The run functions</h2>

  <p>The <code>run</code> family of functions run our audio and produces an unsubscribe function that we use to stop the audio. In this case, <code>run2_</code> does three extra things:
  <ul>
    <li>Wires up our session for two-channel audio. If the sources are mono, it will automatically scale them up to stereo.</li>
    <li>Automatically handles creation and destruction of audio contexts.</li>
    <li>Takes care of the subscription to the rendering engine.</li>
  </ul></p>

  <p>In more advanced cases, like for example when we're controlling MIDI, we'll use other flavors of the <code>run</code> function.</p>

  <p>The <code>push</code> function comes from the <a href="https://github.com/mikesol/purescript-deku"><code>purescript-deku</code></a> framework and is used to stash the current unsubscribe closure for whenever we turn off the audio. In React-land and in Halogen, you'd stash this in a state using the setter provided by <code>useState</code>. But use Deku! This documentation is written in it, and it works seamlessly with wags.</p>

  <h2>Our audio</h2>

  <p>Our audio is expressed with the statement <code>gain_ 0.05 (sinOsc 440.0 pureOn)</code>. The first function, <code>gain_</code>, creates a gain node with a volume of <code>0.05</code>. In WebAudio, gain ranges from <code>0.0</code> to <code>1.0</code> and can be converted to decibels using the following equation:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>In our case, a gain of <code>0.05</code> is roughly <code>-26dB</code></p>.

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>pureOn</code>, as the default is off for <i>all</i> sound generators in Wags. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore more <a ~next~ style="cursor:pointer;">audio units</a>.</p>
</div>"""

helloWorld
  :: forall event payload
   . Plus event
  => IsEvent event
  => (Page -> Effect Unit)
  -> Element event payload
helloWorld dpage = px ~~
  { code: nut
      ( D.pre_
          [ D.code_
              [ text_
                  """case e of
  Just x -> x *> push Nothing
  _ -> run2_ [ gain_ 0.05 [ sinOsc 440.0 pureOn ] ]
         >>= Just >>> push"""
              ]
          ]
      )
  , result: nut
      ( bang (unit /\ Sg.InsertOrUpdate unit)
          @@ \_ -> mkExists $ SubgraphF \push event' ->
            let
              event = (compact (map hush event') <|> bang Nothing)
            in
              D.button
                ( map
                    ( \e -> D.OnClick :=
                        ( cb $
                            (const $ log "hello" *> case e of
                                Just x -> x *> push Nothing
                                _ -> run2_ [ gain_ 0.05 [ sinOsc 440.0 pureOn ] ]
                                       >>= Just >>> push
                            )
                        )
                    )
                    event
                )
                [ text (map (maybe "Turn on" (const "Turn off")) event) ]
      )
  , next: bang (D.OnClick := (cb (const $ dpage AudioUnits *> scrollToTop)))
  }