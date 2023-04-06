module Ocarina.Example.Docs.Params where

import Prelude

import Deku.Core (Nut)
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Event (Event)
import Ocarina.Example.Docs.Params.Cancel as Cancel
import Ocarina.Example.Docs.Params.Envelope as Envelope
import Ocarina.Example.Docs.Params.Numeric as Numeric
import Ocarina.Example.Docs.Params.Sudden as Sudden
import Ocarina.Example.Docs.Params.Unit as Unit
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import Ocarina.Example.Docs.Util (ccassp, mkNext, scrollToTop)
import Type.Proxy (Proxy(..))

px = Proxy :: Proxy """<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how ocarina exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>"""

params :: CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent  -> Nut
params cca' dpage ssp ev = px ~~
  { sudden: Sudden.suddenEx ccb dpage ev
  , numeric: Numeric.numericEx ccb dpage ev
  , envelope: Envelope.envelopeEx ccb dpage ev
  , cancel: Cancel.cancelEx ccb dpage ev
  , unit: Unit.unitEx ccb dpage ev
  , next: mkNext ev cpage
  }
  where
  cpage = dpage State *> scrollToTop
  ccb = ccassp cca' ssp