module Ocarina.Example.Docs.Events.Ex1TL where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, useState', (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Poll (create)
import Ocarina.Control (loopBuf)
import Ocarina.Core (bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Math (calcSlope)
import Ocarina.Properties (loopEnd, loopStart, playbackRate)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)
import QualifiedDo.Alt as OneOf
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3" :: String

main :: Effect Unit
main = do
  { push, poll } <- liftST create
  runInBody (poll <#~> scene)
  push Nothing
  launchAff_ $ bracketCtx
    \ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: Maybe BrowserAudioBuffer
    -> Nut
  scene = maybe (D.div_ [ text_ "Loading..." ]) \buffer ->
    Deku.do
      setS0 /\ sl0 <- useState'
      setS1 /\ sl1 <- useState'
      setS2 /\ sl2 <- useState'
      setStart /\ start <- useState unit
      setStop /\ stop <- useState'
      let
        music = run2_
          [ loopBuf
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
          ]
      D.div_
        $
          map
            ( \{ l, f } -> D.div_
                [ text_ l
                , D.input
                    [ DA.xtypeRange
                    , DA.min_ "0"
                    , DA.max_ "100"
                    , DA.step_ "1"
                    , DA.value_ "50"
                    , DL.input_
                        ( traverse_
                            (valueAsNumber >=> f)
                            <<< (=<<) fromEventTarget
                            <<< target
                        )
                    ]
                    []
                ]
            )
            [ { l: "Playback rate", f: setS0 }
            , { l: "Loop start", f: setS1 }
            , { l: "Loop end", f: setS2 }
            ] <>

            [ D.button
                [ DL.runOn DL.click $ start $> (music >>= setStop)
                , DL.runOn DL.click $ stop <#> (_ *> setStart unit)
                ]
                [ text $ OneOf.do
                    stop $> "Turn off"
                    start $> "Turn on"
                ]
            ]
