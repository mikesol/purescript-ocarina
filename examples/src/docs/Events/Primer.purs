module Ocarina.Example.Docs.Events.Primer where

import Deku.Pursx (pursx')
import Deku.Core (Nut)
import Type.Proxy (Proxy(..))

type Px =  """<section>

  <h2>Events, a primer</h2>

  <p>The <code>Event</code> and <code>Poll</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a =
    Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype APoll event a =
  APoll (forall b. event (a -> b) -> event b)
type Poll = APoll Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> to which you can push values of type <code>a</code>. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using Ocarina, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will bthe Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Poll</h3>

  <p>
    The <code>Poll</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to una <code>b</code>) and unlocks it with an <code>a</code>. Polls don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Poll (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a poll, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Ocarina, we usually want to observe the poll of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>"""

primer :: Nut
primer = pursx' @"@" @Px
  {
  }
  -- where
  -- mnx i = mkNext ev (dpage i *> scrollToTop)
  -- ccb = ccassp cca' ssp