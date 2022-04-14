module WAGS.Example.Docs.Effects where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Plus (class Plus)
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Deku.Attribute (Cb, cb, (:=))
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Pursx ((~~))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.Types (PageAction, Page(..))
import WAGS.Example.Docs.Util (scrollToTop)

data UIAction = Initial | Loading | Result String

clickCb :: (UIAction -> Effect Unit) -> Cb
clickCb push = cb
  ( const do
      push Loading
      launchAff_ $ do
        result <- AX.request
          ( AX.defaultRequest
              { url = "https://randomuser.me/api/"
              , method = Left GET
              , responseFormat = ResponseFormat.json
              }
          )
        case result of
          Left err -> liftEffect $ push
            $ Result
              ( "GET /api response failed to decode: " <>
                  AX.printError err
              )
          Right response -> liftEffect $ push $ Result $
            stringifyWithIndent 2 response.body
  )

clickText = "Click to get some random user data." :: String

px = Proxy :: Proxy """<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  <h2>Folding</h2>

  <p>The type of <code>fold</code> is:</p>

  <pre><code>fold
    :: forall event a b
    . IsEvent event
    => (a -> b -> b)
    -> event a
    -> b
    -> event b</code></pre>

  <p>Fold starts with some initial state <code>b</code> and, based on incoming events, allows you to change the state.</p>

  <p>One way <code>fold</code> is useful is to retain when certain actions happen. In the following example, we use <code>requestAnimationFrame</code> to animate the audio and we use <code>fold</code> to store the instant at which a user clicked a button. At each click, an ascending sine wave with decreasing volume will trigger, and we'll control both the ascent and the volume using the mutable state.</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <p><code>fold</code> is so powerful because it allows us to localize state to <i>any</i> event. Compared to frameworks that have some form of monolithic, top-level state, <code>fold</code> is a much more practical and performant solution.</p>

  <h2>Fix</h2>

  <p>Fix, like it's equivalent in wags that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>
  <p>However, there's one important circumstance where you need fixed points: when (a) the initial state is in an event; and (b) extracting it is costly. In this case, we want a way to mute <code>a</code> when we don't need it and process it when we do. Enter <code>fix</code>. Because the signal feeds back into itself, we can gate and ungate it based on arbitrary conditions. Another way of thinking about this is that <code>fold</code> starts with a <i>pure</i> state and inserts it into our event stream, whereas <code>fix</code> uses current elements of a stream to interact with future ones.</p>

  <p>As an example, imagine that we have a stream whose current elements somehow determine what future elements will be throttled and which ones will pass through. This is also called <i>debouncing</i>. If we implemented debouncing using <code>fold</code>, we'd have to have a one-to-one relationship between elements in an event stream and how they're throttled. But if the stream becomes more or less dense as a result of throttling, we have a problem: the throttling is a function of the density of the stream, but the density of the stream is by definition a function of throttling. Circular dependencies like this can only be solved by fixed-point operators.</p>

  <p>But don't take my word for it! Let's implement our own debouncer. We'll create a somewhat whacky event where the input from a slider results in multiple events with variable millisecond debouncings and then hit it with an incessant timer that triggers a sound file. You can slide the slider to change the debouncing!</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <p>There's a special type of system where the output is a function of change in input called a <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects</a> and a lot of other neat behaviors that are difficult to produce via other means. In fact, the <a href="https://github.com/mikesol/purescript-behaviors">purescript-behaviors</a> library contains some cool differential equation solvers that are powered by <code>fix</code> under the hood via <a href="https://github.com/paf31/purescript-behaviors/blob/0fc50530f71a3026bba12dd8df435bdbd9ef076a/src/FRP/Behavior.purs#L194"><code>fixB</code></a>. Let's listen to one now that creates smooth ramp-up and ramp-down whenever we click and hold a button.</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next two sections, we'll learn two ways to do that. Let's start with exploring <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>"""

effects :: forall event payload. IsEvent event => Plus event => (Page -> Effect Unit) -> Element event payload
effects dpage = px ~~
  { next: bang (D.OnClick := (cb (const $ dpage Subgraph *> scrollToTop)))
  }