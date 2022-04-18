module WAGS.Example.Docs.Events.Flavors where


import Deku.Core (Element)
import Deku.Pursx (makePursx')
import FRP.Event (Event)
import Type.Proxy (Proxy(..))

px = Proxy :: Proxy
      """<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

</section>"""

flavors :: forall payload. Element Event payload
flavors = makePursx'  (Proxy :: _ "@") px
  {
  }
  -- where
  -- mnx i = mkNext ev (dpage i *> scrollToTop)
  -- ccb = ccassp cca' ssp