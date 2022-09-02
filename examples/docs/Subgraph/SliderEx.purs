module Ocarina.Example.Docs.Subgraph.SliderEx where

import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Domable, vbussed)
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, delay, fold, makeEvent, subscribe)
import FRP.Event.Class (biSampleOn)
import FRP.Event.VBus (V, vbus)
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (Audible, silence, sound, bangOn, dyn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent(..))
import Ocarina.Example.Docs.Util (raceSelf)
import Ocarina.Interpret (close, constant0Hack, context, decodeAudioDataFromUri)
import Ocarina.Run (run2_)
import Type.Proxy (Proxy(..))

px =
  Proxy    :: Proxy      """<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>sound</code> to create a subgraph and <code>silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: ocarina automatically frees up resources when you send the <code>silence</code> event. Note that, once you turn a subraph off with <code>silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
"""

txt :: String
txt =
  """module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable)
import Bolson.Core (envy)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe, delay)

import FRP.Event.VBus (V, vbus)
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import Ocarina.Control (gain_, playBuf)
import Ocarina.Core (Channel(..), dyn, bangOn)
import Ocarina.Interpret (bracketCtx, decodeAudioDataFromUri)
import Ocarina.Run (run2_)
import Ocarina.WebAPI (BrowserAudioBuffer)

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
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \buffer ->
    D.div_ $ pure $ envy $ vbus (Proxy :: _ UIEvents) \push event -> do
      let
        startE = pure unit <|> event.startStop.start
        sl = sampleBy (/\) random
          $ fold (\_ b -> b + 1) event.slider 0
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
                    D.OnInput := cb (const (push.slider unit))
                )
                []
            ]
        , D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                startE $> (music >>= push.startStop.stop)
                event.startStop.stop <#>
                  (_ *> push.startStop.start unit)
            )
            [ text OneOf.do
                startE $> "Turn on"
                event.startStop.stop $> "Turn off"
            ]
        ]
"""

type StartStop = V (start :: Unit, stop :: Effect Unit, loading :: Unit)
type UIEvents = V (startStop :: StartStop, slider :: Unit)

bell :: String
bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"

random :: Behavior Number
random = behavior \e ->
  makeEvent \k -> subscribe e \f ->
    Random.random >>= k <<< f

sgSliderEx
  :: forall lock payload
   . CancelCurrentAudio
  -> (Page -> Effect Unit)
  -> Event SingleSubgraphEvent
  -> Domable lock payload
sgSliderEx ccb _ ev = makePursx' (Proxy :: _ "@") px
  { txt: nut (text_ txt)
  , ex1: nut
      (vbussed (Proxy :: _ UIEvents) \push event -> -- here
          do
            let
              startE = pure unit <|> event.startStop.start
              stopE = event.startStop.stop
              sl = sampleBy (/\) random
                $ fold (\_ b -> b + 1) (event.slider) 0

              music :: forall lock0. _ -> Array (Audible _ lock0 _)
              music buffer =
                [ gain_ 1.0 [ dyn $ map
                    ( \i ->
                        oneOf
                          [ pure $ sound $ playBuf
                              { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                              bangOn
                          , delay 5000 $ pure $ silence
                          ]
                    )
                    sl]
                ]
            D.div_
              [ D.div_
                  [ text_ "Slide me!"
                  , D.input
                      ( oneOfMap pure
                          [ D.Xtype := "range"
                          , D.Min := "0"
                          , D.Max := "100"
                          , D.Step := "1"
                          , D.Value := "50"
                          , D.OnInput := cb (const (push.slider unit))
                          ]
                      )
                      []
                  ]
              , D.button
                  ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                      [ event.startStop.loading $> pure unit
                      , stopE <#>
                          (_ *> (ccb (pure unit) *> push.startStop.start unit))
                      , ( biSampleOn (pure (pure unit) <|> (map (\(SetCancel x) -> x) ev))
                            (startE $> identity)
                        ) <#> \cncl -> do
                          cncl
                          push.startStop.loading unit
                          fib <- launchAff do
                            ctx <- context
                            c0h <- constant0Hack ctx
                            buffer <- decodeAudioDataFromUri ctx bell
                            liftEffect do
                              res' <- run2_ (music buffer)
                              let res = res' *> c0h *> close ctx
                              push.startStop.stop res
                              pure res
                          ccb do
                            push.startStop.start unit
                            launchAff_ $ raceSelf fib
                          pure unit

                      ]
                  )
                  [ text $ oneOf
                      [ map (const "Turn off") stopE
                      , map (const "Turn on") startE
                      ]
                  ]
              ]
      )
  }
