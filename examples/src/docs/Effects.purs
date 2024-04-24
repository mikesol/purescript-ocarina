module Ocarina.Example.Docs.Effects where

import Prelude

import Deku.Core (Nut)
import Effect (Effect)
import Deku.Pursx (pursx)
import FRP.Poll (Poll)
import Ocarina.Example.Docs.Effects.FoldEx as Fold
import Ocarina.Example.Docs.FixEx as Fix
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import Ocarina.Example.Docs.Util (ccassp, mkNext, scrollToTop)


type Px = """<div>
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

effects :: CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Poll SingleSubgraphEvent   -> Nut
effects cca' dpage ssp ev = pursx @Px
  { next: mkNext ev (dpage Subgraph *> scrollToTop)
  , fold: Fold.foldEx ccb dpage ssp ev
  , fix: Fix.fixEx ccb dpage ssp ev
  }
  where
  ccb = ccassp cca' ssp