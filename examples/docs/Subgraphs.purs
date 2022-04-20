module WAGS.Example.Docs.Subgraphs where

import Prelude

import Control.Plus (class Plus)
import Deku.Attribute (cb, (:=))
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Event (Event, class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import WAGS.Example.Docs.Util (scrollToTop)

px =  Proxy :: Proxy """<div>
  <h1>Subgraphs</h1>

  <h2>Making audio even more dynamic</h2>
  <p>
    When we're creating video games or other types of interactive work, it's rare that we'll be able to anticipate the exact web audio graph we'll need for an entire session. As an example, imagine that in a video game a certain sound effects accompany various characters, and those characters come in and out based on your progress through the game. One way to solve this would be to anticipate the maximum number of characters that are present in a game and do a round-robin assignment of nodes in the audio graph as characters enter and leave your game. But sometimes that's not ergonomic, and in certain cases its downright inefficient. Another downside is that it does not allow for specialization of the web audio graph based on new data, like for example a character to play a custom sound once you've earned a certain badge.
  </p>

  <p>
    Subgraphs fix this problem. They provide a concise mechansim to dynamically insert audio graphs based on events.
  </p>

  <h2>Hello subgraph</h2>

  <p>To make a subgraph, you need two things:</p>

  <ol>
    <li>An index type. This type needs to implement the <code>Hashable</code> typeclass. You can use this to customize whatever the resulting subgraph will be.</li>
    <li>The subgraph itself, which is a single audio node that must be prefaced by <code>mkSubgraph</code>.</li>
  </ol>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <p>Note how, in this example, we use a delay to turn off audio nodes. One nice thing about subgraphs is that, when they are removed, their nodes are turned off <i>and</i> their events are cancelled, which means that there will never be a case where a subgraph keeps playing or consuming events after it has been removed.</p>

  <h2>Go forth and be brilliant!</h2>
  <p>And thus ends the first version of the wags documentation. Alas, some features remain undocumented, like audio worklets, an imperative API, and an experimental rendering engine called <code>tumult</code> that allows for efficient "VDOM"-esque audio unit diffing in portions of a graph. At some point I hope to document all of these, but hopefully this should be enough to get anyone interested up and running. If you need to use any of those features before I document them, ping me on the <a href="https://purescript.org/chat">PureScript Discord</a>. Otherwise, happy music making with Wags!</p>
</div>"""

subgraphs :: forall payload. CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent  -> Element Event payload
subgraphs _ dpage _ _ = px ~~
  { -- next: bang (D.OnClick := (cb (const $ dpage Intro *> scrollToTop)))
  }