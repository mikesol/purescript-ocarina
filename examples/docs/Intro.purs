module WAGS.Example.Docs.Intro where

import Prelude

import Control.Plus (class Plus)
import Deku.Attribute (cb, (:=))
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.Types (Navigation, Page(..), CancelCurrentAudio)
import WAGS.Example.Docs.Util (scrollToTop)

px = Proxy :: Proxy """<div>
  <h1>Wags</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-wags">Wags</a>.</p>

  <p>Wags is short for "web audio graphs as a stream." The idea is straightforward: events like mouse clicks or MIDI notes or tweens are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is my favorite browser API. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, like <a href="">video games</a>, <a href="">interactive artwork</a>, <a href="">instruments</a>, <a href="">long-form pieces</a> and <a href="">live coding</a>, a need emerges for powerful abstractions to handle events and scheduling.</p>

  <p>Wags's goal is to be the scheduling layer that Web Audio lacks. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript. In the best of cases, it will lead you to ask and answer questions about the flow of time that will result in creative work you wouldn't have otherwise made.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example written in wags that mixes together bells and pure tones to create a shimmering soundscape. It uses the position of the mouse or your finger on the touch screen to modulate the pitch.</p>

  <p>By the end of this documentation, you'll know all of the concepts you need to create snippets like this. This is here to illustrated the following ideas:
  <ul>
    <li>Wags sticks to vanilla PureScript primitives and patterns.</li>
    <li>Wags is performant, with no audible lag on mobile devices.</li>
    <li>Wags is concise, which allows you to move at the speed of your ideas.</li>
  </ul></p>

  <p>This is the Wags documentation. I hope it gets you started off on the right foot. If you have any questions, feel free ping me on the PureScript Discord.</p>

  <p>This documentation can be found <a href="https://github.com/mikesol/purescript-wags/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, check out the <a ~next~ style="cursor:pointer;">hello world section</a>!</p>
</div>"""

intro :: forall event payload.
  IsEvent event =>
  Plus event =>
  CancelCurrentAudio -> (Page -> Effect Unit) -> event Navigation -> Element event payload
intro _ dpage _ = px ~~
  { next: bang (D.OnClick := (cb (const $ dpage HelloWorld *> scrollToTop))) }