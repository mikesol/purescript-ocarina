module WAGS.Example.Docs.Events where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.Events.Ex0 as Ex0
import WAGS.Example.Docs.Events.Ex1 as Ex1
import WAGS.Example.Docs.Events.Ex2 as Ex2
import WAGS.Example.Docs.Events.Ex3 as Ex3
import WAGS.Example.Docs.Events.Ex4 as Ex4
import WAGS.Example.Docs.Events.Ex5 as Ex5
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

  <h2>Events, a primer</h2>

  <p>The <code>Event</code> and <code>Behavior</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a =
    Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype ABehavior event a =
  ABehavior (forall b. event (a -> b) -> event b)
type Behavior = ABehavior Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> to which you can push values of type <code>a</code>. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using Wags, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will block the Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Wags, we usually want to observe the behavior of things like the mouse's position or an audio buffer's content. We'll do both in the following examples.
  </p>

  @ex0@
  @ex1@
  @ex2@
  @ex3@
  @ex4@
  @ex5@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. In the next section, we'll look at how to <a @next@ style="cursor:pointer;">merge and split audio</a>.</p>
</div>"""

events :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> event SingleSubgraphEvent -> Element event payload
events cca' dpage ssp ev = makePursx'  (Proxy :: _ "@") px
  { next: mnx MultiChannel
  , ex0: nut $ Ex0.ex0 ccb dpage ev
  , ex1: nut $ Ex1.ex1 ccb dpage ev
  , ex2: nut $ Ex2.ex2 ccb dpage ev
  , ex3: nut $ Ex3.ex3 ccb dpage ev
  , ex4: nut $ Ex4.ex4 ccb dpage ev
  , ex5: nut $ Ex5.ex5 ccb dpage ev
  }
  where
  mnx i = mkNext ev (dpage i *> scrollToTop)
  ccb = ccassp cca' ssp