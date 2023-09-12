module Ocarina.Example.Docs.Subgraph.Slider where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Data.Either (Either(..), hush)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState', (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Random (random)
import FRP.Event (filterMap)
import FRP.Event.Class (once)
import FRP.Poll (create)
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (dyn, silence, bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)
import QualifiedDo.Alt as OneOf

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"
    :: String

swap :: forall a b. Either a b -> Either b a
swap (Left l) = Right l
swap (Right r) = Left r

main :: Effect Unit
main = do
  { push, poll } <- liftST create
  runInBody (poll <#~> scene)
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
            turnOn = filterMap hush slider
            turnOff = filterMap (swap >>> hush) slider
            music = run2_
              [ gain_ 1.0
                  [ dyn $ map
                      ( \i ->
                          Tuple
                            (once (filter (_ == i) turnOff) $> silence)
                            ( playBuf
                                { buffer: buffer, playbackRate: 0.7 + i * 2.0 }
                                bangOn
                            )
                      )
                      turnOn
                  ]
              ]
          D.div_
            [ D.div_
                [ text_ "Slide me!"
                , D.input
                    [ DA.xtypeRange
                    , DA.min_ "0"
                    , DA.max_ "100"
                    , DA.step_ "1"
                    , DA.value_ "50"
                    , DL.input_ \_ -> do
                        r <- random
                        setSlider $ Right r
                        launchAff_ do
                          delay (Milliseconds 5000.0)
                          liftEffect $ setSlider $ Left r
                    ]
                    []
                ]
            , D.button
                [ DL.runOn DL.click
                    $ startE $> (music >>= setStop)
                , DL.runOn DL.click
                    $ stop <#> (_ *> setStart unit)
                ]
                [ text OneOf.do
                    startE $> "Turn on"
                    stop $> "Turn off"
                ]
            ]
      ]