module Ocarina.Example.Docs.Events.Ex1 where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Nut, vbussed)
import Deku.DOM as D
import Deku.Pursx (makePursx')
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import FRP.Event.VBus (V)
import Ocarina.Control (loopBuf)
import Ocarina.Core (Audible, bangOn)
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import Ocarina.Example.Docs.Util (raceSelf)
import Ocarina.Interpret (close, constant0Hack, context, decodeAudioDataFromUri)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (loopEnd, loopStart, playbackRate)
import Ocarina.Run (run2)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

px =
  Proxy
    :: Proxy
         """<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Ocarina bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>Ocarina.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Ocarina parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
"""

txt :: String
txt =
  """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (biSampleOn)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (loopEnd, loopStart, playbackRate)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3" :: String

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall payload
     . Maybe BrowserAudioBuffer
    -> Nut Effect payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> pure unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              OneOf.do
                bangOn
                map
                  (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                  sl0
                map
                  (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                  sl1
                map
                  (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                  (biSampleOn sl2 (add <$> (pure 0.0 <|> sl1)))
          ]
      D.div_
        $
          map
            ( \{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( O.oneOfMap pure O.do
                        D.Xtype := "range"
                        D.Min := "0"
                        D.Max := "100"
                        D.Step := "1"
                        D.Value := "50"
                        D.OnInput := cb
                          ( traverse_
                              (valueAsNumber >=> f)
                              <<< (=<<) fromEventTarget
                              <<< target
                          )
                    )
                    []
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                    start $> (music >>= push.startStop.stop)
                    event.startStop.stop <#>
                      (_ *> push.startStop.start unit)
                )
                [ text OneOf.do
                    start $> "Turn on"
                    event.startStop.stop $> "Turn off"
                ]
            ]
"""

type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit, loading :: Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari :: String
atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"

ex1
  :: CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Nut
ex1 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { wagtxt:
      ( text_
          """run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ OneOf.do
      bangOn
      (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (pure 0.0 <|> sl1))"""
      )
  , txt: (text_ txt)
  , ex1:
      ( vbussed (Proxy :: _ UIEvents) \push event -> -- here

          do
            let
              sl0 = event.slider.s0
              sl1 = event.slider.s1
              sl2 = event.slider.s2
              start = event.startStop.start <|> pure unit

              music :: _ -> Audible _ _
              music buffer =
                loopBuf
                  { buffer: buffer
                  , playbackRate: 2.6
                  , loopStart: 0.6
                  , loopEnd: 1.1
                  }
                  $ OneOf.do
                      bangOn
                      (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
                      (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
                      (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> (_ <*> sl2)
                        (add <$> (pure 0.0 <|> sl1))
            D.div_
              $
                map
                  ( \{ l, f } -> D.div_
                      [ text_ l
                      , D.input
                          [O.oneOfMap pure O.do
                              D.Xtype := "range"
                              D.Min := "0"
                              D.Max := "100"
                              D.Step := "1"
                              D.Value := "50"
                              D.OnInput := cb
                                ( traverse_
                                    (valueAsNumber >=> f)
                                    <<< (=<<) fromEventTarget
                                    <<< target
                                )
                          ]
                          []
                      ]
                  )
                  [ { l: "Playback rate", f: push.slider.s0 }
                  , { l: "Loop start", f: push.slider.s1 }
                  , { l: "Loop end", f: push.slider.s2 }
                  ] <>
                  [ D.button
                      [O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                          event.startStop.loading $> pure unit
                          event.startStop.stop <#>
                            (_ *> (ccb (pure unit) *> push.startStop.start unit))
                          ( (start $> identity) <*> (pure (pure unit) <|> (map (\(SetCancel x) -> x) ev))

                          ) <#> \cncl -> do
                            cncl
                            push.startStop.loading unit
                            fib <- launchAff do
                              ctx <- context
                              c0h <- constant0Hack ctx
                              buffer <- decodeAudioDataFromUri ctx atari
                              liftEffect do
                                res' <- run2 ctx [ music buffer ]
                                let res = res' *> c0h *> close ctx
                                push.startStop.stop res
                                pure res
                            ccb do
                              push.startStop.start unit
                              launchAff_ $ raceSelf fib
                            pure unit

                      ]
                      [ text $ OneOf.do
                          map (const "Turn off") event.startStop.stop
                          map (const "Turn on") start
                      ]
                  ]
      )
  }
