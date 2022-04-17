module WAGS.Example.Docs.Events.Ex0 where

import Prelude

import Control.Alt ((<|>))
import Control.Alt (alt, (<|>))
import Control.Plus (class Plus)
import Data.Array.NonEmpty ((..))
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Filterable (partitionMap)
import Data.Int (toNumber)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Profunctor (lcmap)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.Control (text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Deku.Toplevel ((🚀))
import Effect (Effect)
import Effect (Effect)
import FRP.Event (class IsEvent, Event, subscribe)
import FRP.Event.Class (bang)
import FRP.Event.Class (bang, biSampleOn)
import Math (pow)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, gain, sinOsc, (~))
import WAGS.Control (microphone, recorder, singleton, speaker2)
import WAGS.Core (AudioInput)
import WAGS.Core (ai)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import WAGS.Example.Docs.Util (WrapperStates(..), clickCb, mkWrapperEvent)
import WAGS.Interpret (close, context, contextState, effectfulAudioInterpret, getMicrophoneAndCamera, makeFFIAudioSnapshot, mediaRecorderToUrl)
import WAGS.Parameter (AudioEnvelope(..), AudioOnOff(..), _off, _on, apOff, apOn, dt)
import WAGS.Properties (onOff)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserMicrophone, MediaRecorderCb(..))

px =
  Proxy    :: Proxy   """<section>
  <h2>Example 1: Hello events</h2>

  <p>Let's say hi to events! The simplest of events, which we've seen already, are the ones that occur immediately upon subscription. You create those types of events using <code>bang</code>. In this section, we'll use <code>bang</code> to set several different types of values:</p>

  <ul>
    <li><code>AudioEnvelope</code> to create an envelope for the gain node.</li>
    <li><code>AudioOnOff</code> to turn the sine-wave oscillator on and off.</li>
  </ul>

  <pre><code>@txt@</code></pre>

  @ex0@

</section>
"""


type ExampleStates = Either UIEvents WrapperStates

scene
  :: forall payload
   . BrowserMicrophone
  -> MediaRecorderCb
  -> AudioInput D2 "" () Event payload
scene m cb =
  singleton
    (recorder cb (microphone m))


data UIEvents = Init | Start | Stop (Effect Unit)

oon o = bang $ onOff $ AudioOnOff { n: _on, o }
oof o = bang $ onOff $ AudioOnOff { n: _off, o }
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
      gain 0.0 (genv x) (sinOsc (200.0 + i * y) (ooo' x))
  ( strand 0.2 900.0
      ~ strand 0.3 1000.0
      ~ strand 0.45 1200.0
      ~ strand 0.7 1400.0
  )

txt :: String
txt = """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array.NonEmpty ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel ((🚀))
import Effect (Effect)
import FRP.Event.Class (bang)
import Math (pow)
import WAGS.Control (gain_, gain, sinOsc, (~))
import WAGS.Core (ai)
import WAGS.Parameter (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = bang $ onOff $ AudioOnOff { n: _on, o }
-- an event to turn our oscillators off
oof o = bang $ onOff $ AudioOnOff { n: _off, o }
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
      gain 0.0 (env' x) (sinOsc (200.0 + i * y) (ooo' x))
  ( strand 0.2 900.0
  ~ strand 0.3 1000.0
  ~ strand 0.45 1200.0
  ~ strand 0.7 1400.0
  )

main :: Effect Unit
main = Init 🚀 \push event ->
  D.div_
    [ D.button
        ( event <#>
            \e -> D.OnClick := cb
              ( const $ case e of
                  Stop u -> u *> push Start
                  _ -> do
                    r <- run2_ $ gain_ 1.0
                      $ ai
                      -- we create 100 cells
                      $ 0 .. 100 <#> cell
                    push $ Stop r
              )
        )
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ]
"""

ex0
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
ex0 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { txt: nut (text_ txt)
  , ex0: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push -> lcmap (alt (bang Init)) \event -> -- here
              D.div_
                [ D.button
                    ( (biSampleOn event (map Tuple ev)) <#> -- here
                        \((SetCancel c) /\ e) -> D.OnClick := cb -- here
                          ( const $ case e of
                              Stop u -> u *> push Start *> ccb (pure unit) -- here
                              _ -> do
                                c -- here
                                r <- run2_ $ gain_ 1.0
                                  $ ai $ 0 .. 100 <#> cell
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
