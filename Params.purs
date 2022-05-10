module WAGS.Example.Docs.Params where

import Prelude

import Deku.Core (Domable)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (Event)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.Params.Cancel as Cancel
import WAGS.Example.Docs.Params.Envelope as Envelope
import WAGS.Example.Docs.Params.Numeric as Numeric
import WAGS.Example.Docs.Params.Sudden as Sudden
import WAGS.Example.Docs.Params.Unit as Unit
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import WAGS.Example.Docs.Util (ccassp, mkNext, scrollToTop)

px = Proxy :: Proxy """<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how wags exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>"""

params :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> Event SingleSubgraphEvent  -> Domable Effect lock payload
params cca' dpage ssp ev = px ~~
  { sudden: nut $ Sudden.suddenEx ccb dpage ev
  , numeric: nut $ Numeric.numericEx ccb dpage ev
  , envelope: nut $ Envelope.envelopeEx ccb dpage ev
  , cancel: nut $ Cancel.cancelEx ccb dpage ev
  , unit: nut $ Unit.unitEx ccb dpage ev
  , next: mkNext ev cpage
  }
  where
  cpage = dpage State *> scrollToTop
  ccb = ccassp cca' ssp