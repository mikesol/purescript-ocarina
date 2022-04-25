module WAGS.Example.Docs.FixFan.Fix1 where

import Prelude

import Deku.Control (text_)
import Deku.Core (Element)
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Control (delay_, fan1, fix, gain, gain_, highpass_, playBuf)
import WAGS.Core (mix)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (audioWrapper)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (AudioEnvelope(..), bangOn)
import WAGS.Properties as P
import WAGS.Run (run2)

px =
  Proxy    :: Proxy      """<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
"""

dgh d g h i =
  delay_ d [ gain_ g [ highpass_ h i ] ]

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [ 1.0, 1.0, 0.0 ], o: 0.0, d: 8.0 }

fade1 = bang
  $ P.gain
  $ AudioEnvelope { p: [ 1.0, 1.0, 0.0 ], o: 0.0, d: 10.0 }

fix1 :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Element lock payload
fix1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { txt: nut $ text_
      """dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 10.0 }

scene buf = run2_
  [ fan1 (playBuf buf bangOn) \b _ -> mix $ fix
      \g0 -> gain_ 1.0
        [ b
        , dgh 0.15 0.7 1500.0
            [ fix
                \g1 -> gain 1.0 fade1
                  [ dgh 0.4 0.5 2500.0
                      [ g0, g1 ]
                  ]
            ]
        , dgh 0.29 0.85 2000.0
            [ fix
                \g1 -> gain_ 1.0
                  [ dgh 0.6 0.6 3500.0
                      [ g0
                      , ( fix
                            \g2 -> gain 1.0 fade0
                              [ dgh 0.75 0.6 4000.0
                                  [ g1, g2 ]
                              , dgh 0.75 0.55 3000.0 [ b ]
                              ]
                        )
                      ]
                  ]
            ]
        ]
  ]"""
  , ai0: nut
      ( audioWrapper ev ccb (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/178/178660_717950-lq.mp3")
          \ctx buf -> run2 ctx
            [ fan1 (playBuf buf bangOn) \b _ -> mix $ fix
                \g0 -> gain_ 1.0
                  [ b
                  , dgh 0.15 0.7 1500.0
                      [ fix
                          \g1 -> gain 1.0 fade1
                            [ dgh 0.4 0.5 2500.0
                                [ g0, g1 ]
                            ]
                      ]
                  , dgh 0.29 0.85 2000.0
                      [ fix
                          \g1 -> gain_ 1.0
                            [ dgh 0.6 0.6 3500.0
                                [ g0
                                , ( fix
                                      \g2 -> gain 1.0 fade0
                                        [ dgh 0.75 0.6 4000.0
                                            [ g1, g2 ]
                                        , dgh 0.75 0.55 3000.0 [ b ]
                                        ]
                                  )
                                ]
                            ]
                      ]
                  ]
            ]
      )
  }
