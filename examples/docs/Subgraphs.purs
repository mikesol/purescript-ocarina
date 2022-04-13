module WAGS.Example.Docs.Subgraphs where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus)
import Data.Either (hush)
import Data.Exists (mkExists)
import Data.Filterable (class Filterable, compact, partitionMap)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Element, Subgraph, SubgraphF(..))
import Deku.DOM as D
import WAGS.Example.Docs.Types (Page(..))
import WAGS.Example.Docs.Util (scrollToTop)
import Deku.Pursx (nut, (~~))
import Deku.Subgraph (SubgraphAction(..), (@@))
import Effect (Effect)
import FRP.Event (class IsEvent, mapAccum)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))

data UIEvents = UIShown | ButtonClicked | SliderMoved Number
derive instance Eq UIEvents

data Sgs = Sg0 | Sg1
derive instance Eq Sgs
derive instance Ord Sgs
instance Show Sgs where
  show Sg0 = "Sg0"
  show Sg1 = "Sg1"
instance Hashable Sgs where
  hash = show >>> hash

counter :: forall event a. IsEvent event => event a → event Int
counter event = map snd $ mapAccum f event 0
  where
  f a b = (b + 1) /\ (a /\ b)

mySub
  :: forall event payload
   . Filterable event
  => IsEvent event
  => (Sgs -> Effect Unit)
  -> Subgraph Sgs Unit event payload
mySub raise Sg0 = mkExists $ SubgraphF \push event ->
  let
    { left, right } = partitionMap identity event
  in
    D.div_
      [ D.div_
          [ D.button
              (bang $ D.OnClick := cb (const $ raise Sg0))
              [ text_ "Send to B" ]
          , D.div_ [ text (map (append "A: " <<< show) (counter left)) ]
          , D.button
              (bang $ D.OnClick := cb (const $ push unit))
              [ text_ "Send to C" ]
          , D.div_
              [ text
                  ( map (append "C: " <<< show)
                      (map (add 1) (counter right) <|> bang 0)
                  )
              ]
          , D.hr_ []

          ]
      ]
mySub raise Sg1 = mkExists $ SubgraphF \push event ->
  let
    { left, right } = partitionMap identity event
  in
    D.div_
      [ D.div_
          [ D.button
              (bang $ D.OnClick := cb (const $ raise Sg1))
              [ text_ "Send to A" ]
          , D.div_ [ text (map (append "B: " <<< show) (counter (left))) ]
          , D.button
              (bang $ D.OnClick := cb (const $ push unit))
              [ text_ "Send to D" ]
          , D.div_
              [ text
                  ( map (append "D: " <<< show)
                      (map (add 1) (counter right) <|> bang 0)
                  )
              ]
          ]
      ]

px =  Proxy :: Proxy """<div>
  <h1>Subgraphs</h1>

  <h2>Making audio even more dynamic</h2>
  <p>
    When we're creating video games or other types of interactive work, it's rare that we'll be able to anticipate the exact web audio graph we'll need for an entire session. As an example, imagine that in a video game a certain sound effects accompany various characters, and those characters come in and out based on your progress through the game. One way to solve this would be to anticipate the maximum number of characters that are present in a game and do some sort of round-robin assignment of nodes in the audio graph as characters enter and leave your game. But sometimes that's not ergonomic, and in certain cases its downright inefficient. Another downside is that it does not allow for specialization of the web audio graph based on new data, like for example a character to play a custom sound once you've earned a certain badge.
  </p>

  <p>
    Subgraphs fix this problem. They provide a concise and elegant mechansim to dynamically insert new fixed audio graphs based on new input. The other mechanism we will discuss in the next section, tumult, allows you to insert new <i>dynamic</i> audio graphs based on new input. When to use tumult vs subgraph is up to your particular requirements, but my experience dictates a 90/10 rule. 90% subgraph, 10% tumult (but that 10% tumult is like magic stardust, so it's worth it!).
  </p>

  <h2>Hello subgraph</h2>

  <p>To make a subgraph, you need two things:</p>

  <ol>
    <li>An index type. This type needs to implement the <code>Hashable</code> typeclass. You can use this to mega-super-customize whatever the resulting subgraph will be.</li>
    <li>The subgraph itself, which is a single audio node that must be prefaced by <code>mkSubgraph</code>.</li>
  </ol>

  <p>Here's a simple subgraph that is connected to a slider. As you slide through the slider, new nodes are provisioned. Each one has a pseudo-random pitch and uses a randomly chosen oscillator.</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <p>Note how, in this example, we use a delay to turn off audio nodes. One nice thing about subgraphs is that, when they are removed, their nodes are turned off <i>and</i> their events are cancelled, which means that there will never be a case where a subgraph keeps playing or consuming events after it has been removed.</p>

  <h2>Subgraphs inside subgraphs</h2>

  <p>Subgraphs are versatile in that the index can be interpreted as any web audio graph. But they're <i>also</i> versatile in that they can contain arbitrarily many subgraphs within them. In the example below, we get inception-y, where each subgraph contains a copy of itself that is used to create a nested feedback loop as you increase the counter. That's a lot of feedback!</p>

  <p>Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h2>Next steps</h2>
  <p>Subgraphs are great when the domain is known, meaning it's <i>your</i> application and <i>your</i> rules. But what if you have a more open context, like for example a live-coding environment? Often times, these applications simply do not work with fixed graphs. For example, if you're live-coding two deeply-nested graphs and find yourself needing to create a feedback loop between them, you're out of luck. This is when you use the subject of our next section: <a ~next~ style="cursor:pointer;">tumult</a>.</p>
</div>"""

subgraphs :: forall event payload. IsEvent event => Plus event => (Page -> Effect Unit) -> Element event payload
subgraphs dpage = px ~~
  { next: bang (D.OnClick := (cb (const $ dpage Intro *> scrollToTop)))
  }