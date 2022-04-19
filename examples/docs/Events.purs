module WAGS.Example.Docs.Events where

import Prelude

import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.Events.Ex0 as Ex0
import WAGS.Example.Docs.Events.Ex1 as Ex1
import WAGS.Example.Docs.Events.Ex2 as Ex2
import WAGS.Example.Docs.Events.Flavors as Flavors
import WAGS.Example.Docs.Events.InWags as InWags
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
  @inWags@
  @flavors@
  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at how to make events <a @next@ style="cursor:pointer;">stateful</a>.</p>
</div>"""

events :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent -> Element Event payload
events cca' dpage ssp ev = makePursx'  (Proxy :: _ "@") px
  { next: mnx State
  , primer: nut $ Primer.primer
  , inWags: nut $ InWags.inWags
  , flavors: nut $ Flavors.flavors
  , ex0: nut $ Ex0.ex0 ccb dpage ev
  , ex1: nut $ Ex1.ex1 ccb dpage ev
  , ex2: nut $ Ex2.ex2 ccb dpage ev
  }
  where
  mnx i = mkNext ev (dpage i *> scrollToTop)
  ccb = ccassp cca' ssp