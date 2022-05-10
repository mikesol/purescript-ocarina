module WAGS.Example.Docs.Events.Ex0 where

import Prelude

import Control.Alt (alt, (<|>))
import Data.Array ((..))
import Data.Exists (mkExists)
import Data.Int (toNumber)
import Data.Number (pow)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Domable, Element, toDOM)
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import FRP.Event (Event, bus)
import FRP.Event.Class (bang, biSampleOn)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain, gain_, microphone, recorder, sinOsc)
import WAGS.Core (Audible, Node)
import WAGS.Core (AudioEnvelope(..), AudioOnOff(..), _off, _on)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserMicrophone, MediaRecorderCb)

px =
  Proxy    :: Proxy         """<section>
  <h2>Example 1: Hello events</h2>

  <p>Let's say hi to events! The simplest of events, which we've seen already, are the ones that occur <span style="font-weight:800;">now</span>, that is to say, immediately upon subscription. You create those types of events using <code>bang</code>. In this section, we'll use <code>bang</code> to set several different types of values:</p>

  <ul>
    <li><code>AudioEnvelope</code> to create an envelope for the gain node. To construct one, use a record with the following parameters:<ul><li><code>p</code>: a list of numbers that will be interpolated over.</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li><li><code>d</code>: the duration of the envelope.</li></ul></li>
    <li><code>AudioOnOff</code> to turn the sine-wave oscillator on and off. To construct one, use a record with the following parameters:<ul><li><code>n</code>: an enum with the value <code>_on</code>, <code>_off</code> or <code>_onOff</code> (more on this in <a href="#example3">Example 3</a> below).</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li></ul></li>
  </ul>

  <p>After that, in the example below, it's functions all the way down. <code>oon</code> and <code>oof</code> create our on/off events, <code>env</code> creates our gain envelope, <code>ooo'</code> and <code>env'</code> specialize these envelopes to a specific point in time, and <code>cell</code> creates a single cell that we deploy 100 times.</p>

  <p>One important thing to note here is the use of the tie fighter (<code>&lt;|&gt;</code>), aka <code>alt</code>, in the definition of <code>ooo'</code>. The <code>Event</code> type, when <code>alt</code>'d, preserves a before-after relationship of the left and right operands when the operands happen at the same time. This is a bit hackish: the events conceptually happen at the same time, but on our CPU, one has to follow the other. We can use this, however, to make sure that certain events happen in a logical sequence. For example, an <code>off</code> instruction must be issued after an <code>on</code> instruction, which we guarantee by using <code>oon</code> on the left side of the alt. If we did it the other way, the <code>on</code> instruction would be last and we'd wind up with 100 oscillators playing at the same time!</p>

  <p>A last thing to note before the music plays is how scheduling works here. Even though all the events are issued upfront via <code>bang</code>, they schedule things to be played <i>later</i> in the audio context. We'll see more advanced scheduling techniques in the <a href="#example4"><code>requestAnimationFrame</code> example below</a>.</p>

  <pre><code>@txt@</code></pre>

  @ex0@

  <p>Unlike the previous examples, this one and all subsequent ones are "batteries included", meaning they are single-file, self-contained PureScript examples that you can compile and run yourself.</p>

</section>
"""

scene
  :: forall lock payload
   . BrowserMicrophone
  -> MediaRecorderCb
  -> Audible D2 lock payload
scene m cb = recorder cb (microphone m)

data UIEvents = Init | Start | Stop (Effect Unit)

oon o = bang $ onOff $ AudioOnOff { x: _on, o }
oof o = bang $ onOff $ AudioOnOff { x: _off, o }
cell = lcmap toNumber \i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 `pow` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 `pow` i)))
    genv x = bang $ P.gain
      $ AudioEnvelope
          { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
          , o: x + 0.3 * (i * (1.005 `pow` i))
          , d: 0.8
          }
    strand x y =
      gain 0.0 (genv x) [sinOsc (200.0 + i * y) (ooo' x)]
  [strand 0.2 4.0
      , strand 0.3 6.0
      , strand 0.45 14.0
      , strand 0.7 20.0
  ]

txt :: String
txt =
  """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody, runInBody1)
import Effect (Effect)
import FRP.Event (bus)
import FRP.Event.Class (bang)
import Math (pow)
import WAGS.Control (gain_, gain, sinOsc)
import WAGS.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = bang $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = bang $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = bang $ P.gain
  $ AudioEnvelope
      { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
      , d: 0.8
      , o
      }

-- a single cell with four oscillators,
-- each of which have the envelope applied
cell = lcmap toNumber \i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 `pow` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 `pow` i)))
    env' x = env (x + 0.3 * (i * (1.005 `pow` i)))
    strand x y =
      gain 0.0 (env' x) [ sinOsc (200.0 + i * y) (ooo' x) ]
  [ strand 0.2 4.0
  , strand 0.3 6.0
  , strand 0.45 14.0
  , strand 0.7 20.0
  ]

main :: Effect Unit
main = runInBody1 (bus \push -> lcmap (bang Init <|> _) \event ->
  D.div_
    [ D.button
        ( event <#>
            \e -> D.OnClick := cb
              ( const $ case e of
                  Stop u -> u *> push Start
                  _ -> do
                    r <- run2_
                      [ gain_ 1.0
                          -- we create 100 cells
                          $ join
                          $ cell <$> 0 .. 100
                      ]
                    push $ Stop r
              )
        )
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ])
"""

ex0
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
ex0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { txt: nut (text_ txt)
  , ex0: nut
      ( toDOM $ bus \push -> lcmap  (bang Init <|> _) \event -> -- here
            D.div_
              [ D.button
                  ( (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map Tuple event)) <#> -- here
                      \(e /\ c) -> D.OnClick := cb -- here
                        ( const $ case e of
                            Stop u -> u *> push Start *> ccb (pure unit) -- here
                            _ -> do
                              c -- here
                              r <- run2_ [gain_ 1.0
                                $ join
                                $ cell <$> 0 .. 100]
                              ccb (r *> push Start) -- here
                              push $ Stop r
                        )
                  )
                  [ text $ event <#> case _ of
                      Stop _ -> "Turn off"
                      _ -> "Turn on"
                  ]
              ]
      )
  }
