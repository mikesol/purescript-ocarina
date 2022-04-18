module WAGS.Example.Docs.FixFan.Fix1 where

import Prelude

import Control.Plus (class Plus)
import Deku.Control (text_)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Control (delay_, gain, gain_, highpass_, playBuf, (~))
import WAGS.Core (fan, fix, hint, input)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper, ctxAff)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (AudioEnvelope(..), pureOn)
import WAGS.Properties as P
import WAGS.Run (run2_)

px =
  Proxy    :: Proxy         """<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
"""

dgh d g h i =
  delay_ d (gain_ g (highpass_ h i))

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [ 1.0, 1.0, 0.0 ], o: 0.0, d: 8.0 }

fade1 = bang
  $ P.gain
  $ AudioEnvelope { p: [ 1.0, 1.0, 0.0 ], o: 0.0, d: 10.0 }

fix1 :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
fix1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { txt: nut $ text_
      """dgh d g h i =
  delay_ d (gain_ g (highpass_ h i))

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 10.0 }

scene buf = run2_
  $ fan (playBuf buf pureOn) \b -> fix
      \g0 -> gain_ 1.0
        ( input b
            ~ dgh 0.15 0.7 1500.0
                ( hint g0 $ fix
                    \g1 -> gain 1.0 fade1
                      $ dgh 0.4 0.5 2500.0
                      $ input g0 ~ input g1
                )
            ~ dgh 0.29 0.85 2000.0
                ( hint g0 $ fix
                    \g1 -> gain_ 1.0
                      $ dgh 0.6 0.6 3500.0
                      $ input g0 ~
                          ( hint g1 $ fix
                              \g2 -> gain 1.0 fade0
                                ( dgh 0.75 0.6 4000.0
                                    (input g1 ~ input g2)
                                    ~ dgh 0.75 0.55 3000.0 (input b)
                                )
                          )
                )
        )"""
  , ai0: nut
      ( audioWrapper ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
          \buf -> run2_
            $ fan (playBuf buf pureOn) \b -> fix
                \g0 -> gain_ 1.0
                  ( input b
                      ~ dgh 0.15 0.7 1500.0
                          ( hint g0 $ fix
                              \g1 -> gain 1.0 fade1
                                $ dgh 0.4 0.5 2500.0
                                $ input g0 ~ input g1
                          )
                      ~ dgh 0.29 0.85 2000.0
                          ( hint g0 $ fix
                              \g1 -> gain_ 1.0
                                $ dgh 0.6 0.6 3500.0
                                $ input g0 ~
                                    ( hint g1 $ fix
                                        \g2 -> gain 1.0 fade0
                                          ( dgh 0.75 0.6 4000.0
                                              (input g1 ~ input g2)
                                              ~ dgh 0.75 0.55 3000.0 (input b)
                                          )
                                    )
                          )
                  )
      )
  }
