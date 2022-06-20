module Ocarina.Example.Docs.Events.Ex2 where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import QualifiedDo.OneOfMap as O
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Domable, envy)
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)
import FRP.Event.Class (bang, biSampleOn)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (interval)
import Ocarina.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import Ocarina.Core (Audible, AudioEnvelope(..), bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import Ocarina.Interpret (close, context)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (frequency)
import Ocarina.Properties as P
import Ocarina.Run (run2e)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

px =
  Proxy    :: Proxy         """<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Ocarina comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Ocarina <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in ocarina is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The ocarina interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The ocarina interval needs an audio context to work.</li>
    <li>The ocarina interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom behavior called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-clock timing, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the clock as a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
"""

txt :: String
txt =
  """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import QualifiedDo.OneOfMap as O
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)
import FRP.Event.Class (bang)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import Ocarina.Clock (interval)
import Ocarina.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import Ocarina.Core (Audible, AudioEnvelope(AudioEnvelope), bangOn)
import Ocarina.Interpret (close, context)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (frequency)
import Ocarina.Properties as P
import Ocarina.Run (run2e)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  )

random :: Behavior Number
random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

-- pentatonic scale
cp :: Number -> Number
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \push event -> do
      let
        start = event.startStop.start <|> bang unit

        music :: forall lock. _ -> Event (Array (Audible _ lock _))
        music evt' = memoize evt' \evt -> do
          let
            pitch = map fst evt
            -- to avoid artifacts in the pitch change
            time = map (add 0.01 <<< snd) evt
            e0 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e1 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.3, 0.1, 0.05, 0.01, 0.005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e2 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.15, 0.05, 0.01, 0.005, 0.0005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            f0 = bangOn <|> frequency <<< cp <$> pitch
          [ fan1 (triangleOsc 0.0 f0) \ipt _ -> do
              gain_ 2.0
                [ gain 0.0 (P.gain <$> e0)
                    [ bandpass_
                        { frequency: 1000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e1)
                    [ bandpass_
                        { frequency: 2000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e2)
                    [ highpass_
                        { frequency: 4000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                ]
          ]
      D.div_
        [ D.div_
            [ text_ "tempo"
            , D.input
                ( O.oneOfMap bang O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb
                      ( traverse_
                          (valueAsNumber >=> push.slider)
                          <<< (=<<) fromEventTarget
                          <<< target
                      )
                )
                []
            ]
        , D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ start $> do
                    ctx <- context
                    let
                      myIvl = sampleBy Tuple random
                        $ interval ctx 0.91
                        $ map (calcSlope 0.0 0.42 100.0 1.4)
                        $ event.slider
                    r <- run2e ctx (music myIvl)
                    push.startStop.stop (r *> close ctx)
                , event.startStop.stop <#>
                    (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ start $> "Turn on"
                , event.startStop.stop $> "Turn off"
                ]
            ]
        ]
  )"""

type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  )

random :: Behavior Number
random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

-- pentatonic scale
cp :: Number -> Number
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

ex2
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable Effect lock payload
ex2 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { txt: nut (text_ txt)
  , ex2: nut
      ( envy $ vbus (Proxy :: _ UIEvents) \push event -> -- here
          let
            start = event.startStop.start <|> bang unit

            music :: forall lock0. _ -> Event (Array (Audible _ lock0 _))
            music evt' = memoize evt' \evt -> do
              let
                pitch = map fst evt
                -- to avoid artifacts in the pitch change
                time = map (add 0.01 <<< snd) evt
                e0 =
                  AudioEnvelope <<<
                    { p: [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                    , d: 0.4
                    , o: _
                    } <$> time
                e1 =
                  AudioEnvelope <<<
                    { p: [ 0.0, 0.3, 0.1, 0.05, 0.01, 0.005, 0.0 ]
                    , d: 0.4
                    , o: _
                    } <$> time
                e2 =
                  AudioEnvelope <<<
                    { p: [ 0.0, 0.15, 0.05, 0.01, 0.005, 0.0005, 0.0 ]
                    , d: 0.4
                    , o: _
                    } <$> time
                f0 = bangOn <|> frequency <<< cp <$> pitch
              [ fan1 (triangleOsc 0.0 f0) \ipt _ -> do
                  gain_ 2.0
                    [ gain 0.0 (P.gain <$> e0)
                        [ bandpass_
                            { frequency: 1000.0
                            , q: 20.0
                            }
                            [ ipt ]
                        ]
                    , gain 0.0 (P.gain <$> e1)
                        [ bandpass_
                            { frequency: 2000.0
                            , q: 20.0
                            }
                            [ ipt ]
                        ]
                    , gain 0.0 (P.gain <$> e2)
                        [ highpass_
                            { frequency: 4000.0
                            , q: 20.0
                            }
                            [ ipt ]
                        ]
                    ]
              ]
          in
            D.div_
              [ D.div_
                  [ text_ "tempo"
                  , D.input
                      ( O.oneOfMap bang O.do
                          D.Xtype := "range"
                          D.Min := "0"
                          D.Max := "100"
                          D.Step := "1"
                          D.Value := "50"
                          D.OnInput := cb
                            ( traverse_
                                (valueAsNumber >=> push.slider)
                                <<< (=<<) fromEventTarget
                                <<< target
                            )
                      )
                      []
                  ]
              --                     r' <- run2e ctx (music myIvl)
              -- let r = r' *> close ctx
              -- ccb (r *> push start) -- here
              -- push (stop r)

              , D.button
                  ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                      [ (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (start $> identity)) <#> \cncl -> do
                          cncl
                          ctx <- context
                          let
                            myIvl = sampleBy Tuple random
                              $ interval ctx 0.91
                              $ map (calcSlope 0.0 0.42 100.0 1.4)
                              $ event.slider
                          r' <- run2e ctx (music myIvl)
                          let r = r' *> close ctx
                          ccb (r *> push.startStop.start unit)
                          push.startStop.stop (r *> close ctx)
                      , event.startStop.stop <#>
                          (_ *> (ccb (pure unit) *> push.startStop.start unit))
                      ]
                  )
                  [ text $ oneOf
                      [ start $> "Turn on"
                      , event.startStop.stop $> "Turn off"
                      ]
                  ]
              ]
      )
  }