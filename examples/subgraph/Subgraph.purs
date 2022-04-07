module WAGS.Example.Subgraph where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Either (either)
import Data.Exists (Exists, mkExists)
import Data.Foldable (for_)
import Data.Lens (_1, over)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, text, text_)
import Deku.Core (SubgraphF(..))
import Deku.DOM as DOM
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph as Sg
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (Event, keepLatest, mapAccum, sampleOn, subscribe)
import FRP.Event.Phantom (PhantomEvent, proof0, toEvent)
import FRP.Event.Time (interval)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain, gain', gain__, highpass, input, loopBuf, lowpass, sinOsc, speaker2, (:*))
import WAGS.Core (GainInput, Input, Subgraph(..))
import WAGS.Example.Utils (RaiseCancellation, animationFrameEvent)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, effectfulAudioInterpret, makeFFIAudioSnapshot, writeHead)
import WAGS.Parameter (ACTime, ovnn, pureOn, uat_)
import WAGS.Properties (frequency)
import WAGS.Subgraph as Wsg
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
  :: forall proof payload
   . BrowserAudioBuffer
  -> PhantomEvent proof (ACTime /\ Int)
  -> GainInput D2 (toSubg :: Input) (toSubg :: Input) PhantomEvent proof payload
scene loopy wh =
  let
    topE = map (over _1 (flip uat_ (mul pi))) wh
  in
    gain__ 0.0 empty
      ( gain' (Proxy :: _ "toSubg") 1.0 empty
          (loopBuf loopy pureOn :* [])
      ) :*
      [ Wsg.subgraph
          ( keepLatest $ map
              ( \(tr' /\ ix) ->
                  ( pure (ix /\ Wsg.InsertOrUpdate tr') <|> pure
                      ((ix - 1) /\ Wsg.Remove)
                  )
              )
              topE
          )
          ( \({ toSubg } :: { toSubg :: Input }) -> Subgraph \ix tr ->
              let
                ooo
                  | ix == 0 =
                      gain 1.0 empty
                        ( highpass 1100.0
                            ( map
                                ( frequency <<< ovnn
                                    (\x -> 3100.0 + 1000.0 * sin (0.5 * x))
                                )
                                tr
                            )
                            (input toSubg) :*
                            [ gain__ 0.03 empty (sinOsc 220.0 pureOn) ]
                        )

                  | otherwise =
                      gain 1.0 empty
                        ( lowpass 1100.0
                            ( map
                                ( frequency <<< ovnn
                                    (\x -> 1100.0 + 1000.0 * sin (0.5 * x))
                                )
                                tr
                            )
                            (input toSubg) :*
                            [ gain__ 0.03 empty
                                ( sinOsc 820.0
                                    ( pureOn <|>
                                        ( map
                                            ( frequency <<< ovnn
                                                ( \x -> 1100.0 + 1000.0 * sin
                                                    (0.5 * x)
                                                )
                                            )
                                            tr
                                        )
                                    )
                                )
                            ]
                        )
              in
                ooo
          )
      ]

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

type Init = BrowserAudioBuffer

initializeSubgraph :: Aff Init
initializeSubgraph = do
  atar <- liftEffect context >>= flip decodeAudioDataFromUri
    "https://freesound.org/data/previews/36/36132_321601-hq.mp3"
  pure atar

subgraphExample
  :: forall payload
   . BrowserAudioBuffer
  -> RaiseCancellation
  -> Exists (SubgraphF Unit PhantomEvent payload)
subgraphExample loopy rc = mkExists $ SubgraphF \push -> lcmap
  (map (either (const Nothing) identity))
  \event ->
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
                            unsub <- subscribe
                              ( toEvent $ speaker2
                                  ( scene loopy
                                      ( proof0
                                          ( sampleOn
                                              ( ( map (add 1) $ counter_
                                                    (interval 3000)
                                                ) <|> pure 0
                                              )
                                              ( map Tuple $ sample_ wh
                                                  animationFrameEvent
                                              )
                                          )
                                      )
                                  )
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
          ( Sg.subgraph (pure (Tuple unit (Sg.InsertOrUpdate unit)))
              (const $ subgraphExample init (const $ pure unit))
          )
          effectfulDOMInterpret
      _ <- subscribe (toEvent evt) \i -> i ffi
      pure unit