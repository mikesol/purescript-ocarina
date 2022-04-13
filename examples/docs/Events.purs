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

  <p>The <code>Event</code> and <code>Behavior</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a= Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype ABehavior event a = ABehavior (forall b. event (a -> b) -> event b)
type Behavior = ABehavior Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> that you can push values of type <code></code> to to your heart's content. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using wags, you have to get your events from somewhere. At a minimum, you'll consume an event that turns on audio as the result of a browser interaction. Without this event, most browsers will block your audio from turning on.
  </p>
  <p>
    Events are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This has a deep connection to the real world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite. Behaviors encapsualte this elegantly.
  </p>

  <p>
    In wags, while you can of course observe the behavior of the weather or the Earth's rotation, it's usually more interesting to observe the behavior of things like the mouse's position or an audio buffer's content. We'll do both in the following examples.
  </p>

  <h2>Example 1: Three sliders</h2>

  <p>In this example, we'll use a slider to control the playback rate of a buffer. Unlike the previous examples, this one and all subsequent ones will come "batteries included", meaning they'll be self-contained PureScript files that you can compile and run yourself on this <a>trypurescript</a> instance.</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <p>Note that our <code>loopBuf</code> consumes <i>four</i> events: in addition to the three sliders, there is a <code>pureOn</code> event that turns it on. <i>On</i> and <i>off</i> states can be controlled with events as well, as we'll see in the example below.</p>

  <h2>Example 2: A counter</h2>

  <p>In the following example, we emit as many sounds as clicks. So when we click once, we hear one sound, when we click twice, we hear two sounds, when we click thrice, we hear three sounds, and so forth and so on.</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <p>The important thing to note in this example is the use of <code>on</code>, <code>off</code>, and <code>offOn</code>. They work as follows:</p>

  <table>
    <tr>
      <th>Command</th>
      <th>If currently on</th>
      <th>If currently off</th>
    </tr>
    <tr>
      <td><code>on</code></td>
      <td>Does nothing</td>
      <td>Turns on</td>
    </tr>
    <tr>
      <td><code>off</code></td>
      <td>Turns off</td>
      <td>Does nothing</td>
    </tr>
    <tr>
      <td><code>offOn</code></td>
      <td>Restarts the buffer</td>
      <td>Starts the buffer</td>
    </tr>
  </table>

  <p>We aslo see that on/off events can be scheduled in the future as well using the <code>o</code> parameter of <code>AudioOnOff</code>.</p>

  <h2>Example 3: requestAnimationFrame</h2>

  <p>When you're working with canvas animations, you'll almost always use <code>requestAnimationFrame</code> to create a loop for the animation. You can use <code>requestAnimationFrame</code> to "animate" your audio as well.</p>

  <blockquote>While you <i>can</i> use <code>requestAnimationFrame</code> to animate audio, it comes at a cost: it eats up power. Modern browsers these days are super efficient, so the power consumption is way better than in the bad old days. But if you're making an interactive audio work, consider using <code>requestAnimationFrame</code> only when you need it.</blockquote>

  <p>In the example below, we animate 100 filtered oscillators in an upward spiral using <code>requestAnimationFrame</code>. If you do a performance trace, you'll see that even for something this involved, a modern desktop computer will be in the idle state for over 95% of the time, which is grrreat!</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <p>In this example, we used a small performance optimization where we turn off sine wave oscillators that are not used. Even though this is not strictly necessary, as we are gating everything with gain nodes, turning off unused oscialltors will lower power consumption by 10-50%.</p>

  <h2>Example 4: Following the mouse</h2>

  <p>This example shows how to respond to the mouse's position. Rather than using an event listener, which can fire way too fast than is necessary for rendering, we'll use a behavior with the mouse's position and sample it via <code>requestAnimationFrame</code>.</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h2>Example 5: An audio player with behaviors</h2>

  <p>Behaviors are a great way to create interactive works where user actions are decoupled from rendering. In the following example, the rendering is consistently once a second, but the audio buffer be changed as often or as little as we want. The ticker event samples the buffer behavior to grab the current buffer.</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior types</code>. In the next section, we'll build more complex signal processing flows using a pair of functions called <a ~next~ style="cursor:pointer;"><code>fix</code> and <code>fan</code></a>.</p>
</div>"""

events :: forall event payload. IsEvent event => Plus event => (Page -> Effect Unit) -> Element event payload
events dpage = px ~~
  { next: bang (D.OnClick := (cb (const $ dpage FixFan *> scrollToTop)))
  }