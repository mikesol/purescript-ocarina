module WAGS.Example.Docs.Pursx2 where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import WAGS.Example.Docs.Types (Page)
import Deku.Pursx (makePursx')
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))

px =
  Proxy   :: Proxy      """<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out wags! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-wags">Wags GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Happy wagging!</p>
</div>"""


pursx2
  :: forall event payload
   . IsEvent event
  => Plus event
  => (Page -> Effect Unit)
  -> Element event payload
pursx2 dpage = makePursx' (Proxy :: _ "~") px
  { }