module Ocarina.Example.Docs.Events.Ex0 where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Number (pow)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Pursx (pursx')
import Effect (Effect)
import FRP.Poll (Poll)
import Ocarina.Control (gain, gain_, microphone, recorder, sinOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), AudioOnOff(AudioOnOff), _off, _on, AudioEnvelope(..), AudioOnOff(..), _off, _on)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserMicrophone, MediaRecorderCb)

scene
  :: forall payload
   . BrowserMicrophone
  -> MediaRecorderCb
  -> Audible D2 payload
scene m cb = recorder cb (microphone m)

data UIEvents = Init | Start | Stop (Effect Unit)

oon o = pure $ onOff $ AudioOnOff { x: _on, o }
oof o = pure $ onOff $ AudioOnOff { x: _off, o }
cell = lcmap toNumber \i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 `pow` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 `pow` i)))
    genv x = pure $ P.gain
      $ AudioEnvelope
          { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
          , o: x + 0.3 * (i * (1.005 `pow` i))
          , d: 0.8
          }
    strand x y =
      gain 0.0 (genv x) [ sinOsc (200.0 + i * y) (ooo' x) ]
  [ strand 0.2 4.0
  , strand 0.3 6.0
  , strand 0.45 14.0
  , strand 0.7 20.0
  ]

ex0
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Poll SingleSubgraphEvent -> Nut
ex0 ccb _ ev = pursx' @"@" @Px
  { txt: (text_ txt)
  , ex0: Deku.do
      push /\ event <- useState Init

      D.div_
        [ D.button
            [ DL.runOn DL.click $ (Tuple <$> event <*> (pure (pure unit) <|> (map (\(SetCancel x) -> x) ev))) <#>
                \(e /\ c) -> case e of
                  Stop u -> u *> push Start *> ccb (pure unit)
                  _ -> do
                    c -- here
                    r <- run2_
                      [ gain_ 1.0
                          $ join
                          $ cell <$> 0 .. 2 -- 100
                      ]
                    ccb (r *> push Start) -- here
                    push $ Stop r

            ]
            [ text $ event <#> case _ of
                Stop _ -> "Turn off"
                _ -> "Turn on"
            ]
        ]

  }

type Px = """<section>
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
--------------
--------------
--------------

txt :: String
txt =
  """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Number (pow)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Ocarina.Control (gain_, gain, sinOsc)
import Ocarina.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import Ocarina.Properties (onOff)
import Ocarina.Properties as P
import Ocarina.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = pure $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = pure $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = pure $ P.gain
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
main = runInBody Deku.do
  push /\ event <- useState Init
  D.div_
    [ D.button
        [ DL.runOn DL.click $ event <#> case _ of
            Stop u -> u *> push Start
            _ -> do
              r <- run2_
                [ gain_ 1.0
                    -- we create 100 cells
                    $ join
                    $ cell <$> 0 .. 100
                ]
              push $ Stop r
        ]
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ]
"""