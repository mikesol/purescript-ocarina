module Ocarina.Example.Docs.AudioUnits.Constant where

import Prelude


import Control.Alt ((<|>))
import Data.Array ((..))
import Data.FunctorWithIndex (mapWithIndex)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.Pursx (pursx)
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (gain_, constant)
import Ocarina.Core (AudioEnvelope(..), bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (audioWrapper)
import Ocarina.Properties (offset)
import Ocarina.Run (run2)

type Px = """<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Ocarina uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>pure</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
"""

constantEx
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
constantEx ccb _ ev = pursx @Px
  { tf: (text_ "<|>")
  , txt:
      ( text_
          """run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( pure $ offset $ AudioEnvelope
                  { d: 5.0
                  , o: 0.1
                  , p: 0 .. 1920 # mapWithIndex
                      \i -> const $
                      if i `mod` 3 == 0 then 1.0
                      else 0.0
                  }
              )
          )
      ]
  ]"""
      )
  , constant:
      ( audioWrapper ev ccb (\_ -> pure unit) \ctx _ -> run2 ctx
          [ gain_ 0.5
              [ constant 0.0
                  ( bangOn <|>
                      ( pure $ offset $ AudioEnvelope
                          { d: 5.0
                          , o: 0.1
                          , p: 0 .. 1920 # mapWithIndex
                              \i -> const $
                                if i `mod` 3 == 0 then 1.0
                                else 0.0
                          }
                      )
                  )
              ]
          ]
      )
  }