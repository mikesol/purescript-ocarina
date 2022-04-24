module WAGS.Example.Subgraph where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Plus (empty)
import Data.Exists (Exists, mkExists)
import Data.Foldable (for_)
import Data.Lens (over)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, text, text_)
import Deku.Core (SubgraphF(..))
import Deku.DOM as DOM
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (Event, Event, fold, keepLatest, mapAccum, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang)
import FRP.Event.Time (interval)
import Math (pi, sin)
import WAGS.Clock (WriteHead, fot, writeHead)
import WAGS.Control (convolver, gain, highpass, loopBuf, lowpass, sinOsc, speaker2, triangleOsc)
import WAGS.Core (InitializeConvolver(..), Node, fan, input, mkSubgraph, subgraph)
import WAGS.Core as C
import WAGS.Example.Utils (RaiseCancellation)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, effectfulAudioInterpret, makeFFIAudioSnapshot)
import WAGS.Parameter (opticN, bangOn)
import WAGS.Properties (frequency)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

counter :: forall a. Event a → Event (Tuple a Int)
counter event = mapAccum f event 0
  where
  f a b = Tuple (b + 1) (Tuple a b)

counter_ :: forall a. Event a -> Event Int
counter_ = map snd <<< counter

scene
  :: forall lock payload
   . Init
  -> WriteHead Event
  -> Array (Node D2 lock Event payload)
scene { loopy, conny } wh =
  let
    tr = fot wh (mul pi)
  in
    pure $ fan (gain 1.0 empty [ loopBuf loopy bangOn ]) \(toSubg) ->
      subgraph
        ( keepLatest $ map
            ( \ix ->
                ( bang (ix /\ C.Insert) <|> bang
                    ((ix - 1) /\ C.Remove)
                )
            )
            ( fold (\_ b -> b + 1)
                (interval 3000 $> unit <|> bang unit)
                (-1)
            )
        )
        ( \ix ->
            let
              ooo
                | ix == 0 =
                    mkSubgraph $ convolver (InitializeConvolver { buffer: conny })
                      [ gain 1.0 empty
                          [ highpass 1100.0
                              ( map
                                  ( frequency <<< over opticN
                                      ( \x -> 3100.0 + 1000.0 * sin
                                          (0.5 * x)
                                      )
                                  )
                                  tr
                              )
                              [ toSubg ]
                          , gain 0.03 empty [ sinOsc 220.0 bangOn ]
                          ]
                      ]
                | ix == 1 =
                    mkSubgraph $ gain 1.0 empty
                      [ highpass 2200.0
                          ( map
                              ( frequency <<< over opticN
                                  ( \x -> 2200.0 + 1000.0 * sin
                                      (0.5 * x)
                                  )
                              )
                              tr
                          )
                          [ toSubg ]
                      , gain 0.03 empty
                          [ triangleOsc 2000.0
                              ( bangOn <|>
                                  ( map
                                      ( frequency <<< over opticN
                                          ( \x -> 2000.0 + 300.0 * sin
                                              (0.5 * x)
                                          )
                                      )
                                      tr
                                  )
                              )
                          ]
                      ]
                | otherwise =
                    mkSubgraph $ gain 1.0 empty
                      [ lowpass 1100.0
                          ( map
                              ( frequency <<< over opticN
                                  ( \x -> 1100.0 + 1000.0 * sin
                                      (0.5 * x)
                                  )
                              )
                              tr
                          )
                          [ toSubg ]
                      , gain 0.03 empty
                          [ sinOsc 820.0
                              ( bangOn <|>
                                  ( map
                                      ( frequency <<< over opticN
                                          ( \x -> 1100.0 + 1000.0 *
                                              sin
                                                (0.5 * x)
                                          )
                                      )
                                      tr
                                  )
                              )
                          ]
                      ]
            in
              ooo
        )

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

type Init = { loopy :: BrowserAudioBuffer, conny :: BrowserAudioBuffer }

initializeSubgraph :: Aff Init
initializeSubgraph = do
  loopy <- liftEffect context >>= flip decodeAudioDataFromUri
    "https://freesound.org/data/previews/36/36132_321601-hq.mp3"
  conny <- liftEffect context >>= flip decodeAudioDataFromUri
    "https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"
  pure { loopy, conny }

subgraphExample
  :: forall payload
   . Init
  -> RaiseCancellation
  -> Exists (SubgraphF Event payload)
subgraphExample loopy rc = mkExists $ SubgraphF \push -> lcmap (alt (bang Nothing)) \event ->
  DOM.div_
    [ DOM.h1_ [ text_ "Subgraph" ]
    , DOM.button
        ( map
            ( \i -> DOM.OnClick := cb
                ( const $
                    maybe
                      ( do
                          ctx <- context
                          ffi2 <- makeFFIAudioSnapshot ctx
                          let wh = writeHead 0.04 ctx
                          afe <- animationFrameEvent
                          unsub <- subscribe
                            ( speaker2
                                (scene loopy (sample_ wh afe))
                                effectfulAudioInterpret
                            )
                            ((#) ffi2)
                          rc $ Just { unsub, ctx }
                          push $ Just { unsub, ctx }
                      )
                      ( \{ unsub, ctx } -> do
                          rc Nothing
                          unsub
                          close ctx
                          push Nothing
                      )
                      i
                )
            )
            event
        )
        [ text
            (map (maybe "Turn on" (const "Turn off")) event)
        ]
    ]

main :: Effect Unit
main = launchAff_ do
  init <- initializeSubgraph
  liftEffect do
    b' <- window >>= document >>= body
    for_ (toElement <$> b') \elt -> do
      ffi <- makeFFIDOMSnapshot
      let
        evt = deku elt
          ( Sg.subgraph (bang (Tuple unit Sg.Insert))
              (const $ subgraphExample init (const $ pure unit))
          )
          effectfulDOMInterpret
      _ <- subscribe evt \i -> i ffi
      pure unit