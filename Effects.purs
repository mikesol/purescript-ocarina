module WAGS.Example.Docs.Effects where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus)
import Data.Exists (mkExists)
import Data.Foldable (oneOf)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, match)
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))

import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (Event, filterMap, fold, mapAccum, sampleOn)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (class IsEvent, bang, biSampleOn)

import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain, periodicOsc)
import WAGS.Example.Docs.Effects.FoldEx as Fold
import WAGS.Example.Docs.FixEx as Fix
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent(..), SingleSubgraphPusher)
import WAGS.Example.Docs.Util (ccassp, mkNext, scrollToTop)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2)


px = Proxy :: Proxy """<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>"""

effects :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent   -> Element lock payload
effects cca' dpage ssp ev = px ~~
  { next: mkNext ev (dpage Subgraph *> scrollToTop)
  , fold: nut $ Fold.foldEx ccb dpage ssp ev
  , fix: nut $ Fix.fixEx ccb dpage ssp ev
  }
  where
  ccb = ccassp cca' ssp