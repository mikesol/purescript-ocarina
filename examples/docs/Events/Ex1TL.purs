module WAGS.Example.Docs.Events.Ex1TL where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, plant, switcher, text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (bang, biSampleOn, keepLatest)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Parameter (bangOn)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserAudioBuffer)
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
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Element lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \buffer ->
    D.div_ $ keepLatest $ vbus (Proxy :: _ UIEvents) \push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> bang unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              $ oneOf
                [ bangOn
                , map
                    (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                    sl0
                , map
                    (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                    sl1
                , map
                    (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                    (biSampleOn sl2 (add <$> (bang 0.0 <|> sl1)))
                ]
          ]
      plant $ D.div_
        $
          map
            ( \{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( oneOfMap bang
                        [ D.Xtype := "range"
                        , D.Min := "0"
                        , D.Max := "100"
                        , D.Step := "1"
                        , D.Value := "50"
                        , D.OnInput := cb
                            ( traverse_
                                (valueAsNumber >=> f)
                                <<< (=<<) fromEventTarget
                                <<< target
                            )
                        ]
                    )
                    blank
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                    [ start $> (music >>= push.startStop.stop)
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