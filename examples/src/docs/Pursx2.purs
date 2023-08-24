module Ocarina.Example.Docs.Pursx2 where

import Prelude

import Deku.Core (Nut)
import Deku.Pursx (makePursx')
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent, SingleSubgraphPusher)
import Type.Proxy (Proxy(..))

px =
  Proxy   :: Proxy      """<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out ocarina! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-ocarina">Ocarina GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Now go out there and play some ocarina!</p>
</div>"""


pursx2
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Poll SingleSubgraphEvent
  -> Nut
pursx2 _ _ _ _ = makePursx' (Proxy :: _ "~") px
  { }