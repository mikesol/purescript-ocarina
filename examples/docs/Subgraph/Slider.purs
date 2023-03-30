module Ocarina.Example.Docs.Subgraph.Slider where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do as Deku
import Deku.Hooks (useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, delay, fold, makeEvent, subscribe)
import FRP.Event.VBus (V)
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (dyn, sound, silence, bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O

type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Unit)

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"
    :: String

random :: Behavior Number
random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene (event))
  push Nothing
  launchAff_ $ bracketCtx
    \ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: Maybe BrowserAudioBuffer
    -> Nut
  scene = maybe (D.div_ [ text_ "Loading..." ]) \buffer ->
    D.div_
      [ Deku.do
          setStart /\ start <- useState'
          setStop /\ stop <- useState'
          setSlider /\ slider <- useState'
          let
            startE = pure unit <|> start
            sl = sampleBy (/\) random
              $ fold (\b _ -> b + 1) 0 (slider)
            music = run2_
              [ gain_ 1.0
                  [ dyn $ map
                      ( \i ->
                          OneOf.do
                            pure $ sound $ playBuf
                              { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                              bangOn
                            delay 5000 $ pure $ silence
                      )
                      sl
                  ]
              ]
          D.div_
            [ D.div_
                [ text_ "Slide me!"
                , D.input
                    ( O.oneOfMap pure O.do
                        D.Xtype := "range"
                        D.Min := "0"
                        D.Max := "100"
                        D.Step := "1"
                        D.Value := "50"
                        D.OnInput := cb (const (setSlider unit))
                    )
                    []
                ]
            , D.button
                ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                    startE $> (music >>= setStop)
                    stop <#>
                      (_ *> setStart unit)
                )
                [ text OneOf.do
                    startE $> "Turn on"
                    stop $> "Turn off"
                ]
            ]
      ]