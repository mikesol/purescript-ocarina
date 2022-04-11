module WAGS.Example.StressTest where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Either (either)
import Data.Exists (Exists, mkExists)
import Data.Filterable (filter, filterMap)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as DOM
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, Event, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang)
import FRP.Event.Memoizable as Memoizable
import FRP.Event.Memoize (class MemoizableEvent, memoizeIfMemoizable)
import FRP.Event.Memoized as Memoized
import Math (pi, sin, (%))
import WAGS.Control (gain__, sinOsc, speaker2, (:*))
import WAGS.Core (GainInput)
import WAGS.Example.Utils (RaiseCancellation)
import WAGS.Interpret (FFIAudioSnapshot, close, context, effectfulAudioInterpret', makeFFIAudioSnapshot, writeHead)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioNumeric(..), AudioOnOff(..), WriteHead, _off, _on, at_, ovnn)
import WAGS.Properties as Common
import WAGS.WebAPI (AudioContext)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

last :: Int
last = 100

len :: Number
len = 12.0

lm2 :: Number
lm2 = len - 2.0

scene
  :: forall event payload
   . IsEvent event
  => MemoizableEvent event
  => WriteHead event
  -> GainInput D2 () () event payload
scene wh =
  let
    tr = memoizeIfMemoizable (at_ wh (mul pi))
    gso a b c st ed = gain__ 0.0
      ( Common.gain <$>
          ( filterMap
              ( \(AudioNumeric x@{ o }) ->
                  let
                    olen = o % len
                  in
                    if olen < ed + 0.6 && olen > ed then
                      ( Just
                          ( AudioNumeric
                              ( x
                                  { n = min a $ max 0.0 $ calcSlope ed a
                                      (ed + 0.5)
                                      0.0
                                      olen
                                  }
                              )
                          )
                      )
                    else if olen < st + 0.5 && olen > st then
                      (Just (AudioNumeric (x { n = a })))
                    else Nothing
              )
              tr
          )
      )
      ( sinOsc b
          ( Common.frequency <<< (ovnn c) <$>
              ( filter
                  (\(AudioNumeric { o }) -> o % len < ed && o % len > st)
                  tr
              )
              <|> Common.onOff <$>
                ( filterMap
                    ( \(AudioNumeric { o }) ->
                        if o % len < st + 0.5 && o % len > st then
                          (Just (AudioOnOff { onOff: _on, timeOffset: o }))
                        else if o % len < ed + 0.5 && o % len > ed then
                          (Just (AudioOnOff { onOff: _off, timeOffset: o }))
                        else Nothing
                    )
                    tr
                )
          )
      )
  in
    gso 0.1 440.0 (\rad -> 440.0 + (10.0 * sin (2.3 * rad))) 0.0 0.2 :*
      ( map
          ( \i ->
              let
                frac = toNumber i / toNumber last
              in
                gso 0.25 (235.0)
                  ( \rad -> (235.0 + frac * 1000.0)
                      +
                        ( (10.0 + frac * 100.0) * sin
                            ((frac * 10.0 + 1.0) * rad)
                        )
                  )
                  (frac * lm2)
                  ((frac * lm2) + 0.3)
          )
          (0 .. last)
      )

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

type Init = Unit

initializeStressTest :: Aff Init
initializeStressTest = pure unit

foreign import stressTest_
  :: forall event. AudioContext -> WriteHead event -> event (FFIAudioSnapshot -> Effect Unit)

stressTest
  :: forall payload
   . Unit
  -> RaiseCancellation
  -> Exists (SubgraphF Unit Event payload)
stressTest _ rc = mkExists $ SubgraphF \p -> lcmap
  (map (either (const Nothing) identity))
  \e ->
    let
      musicButton
        :: forall event
         . IsEvent event
        => String
        -> (Event ~> event)
        -> (event ~> Event)
        -> (UIAction -> Effect Unit)
        -> Event UIAction
        -> (AudioContext -> WriteHead event -> event (FFIAudioSnapshot -> Effect Unit))
        -> Element Event payload
      musicButton label toE fromE push event audioEvent = DOM.button
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
                            (fromE (audioEvent ctx (toE (sample_ wh afe))))
                            ((#) ffi2)
                          rc $ Just { unsub, ctx }
                          push $ Just { unsub, ctx }
                      )
                      ( \{ unsub, ctx } -> do
                          unsub
                          close ctx
                          rc Nothing
                          push Nothing
                      )
                      i
                )
            )
            event
        )
        [ text
            (map (maybe ("Turn on " <> label) (const "Turn off")) event)
        ]
    in
      DOM.div_
        [ DOM.h1_ [ text_ "Stress test" ]
        , musicButton "Event" identity identity p e
            (\_ -> flip speaker2 (effectfulAudioInterpret' identity identity) <<<
                scene
            )
        , musicButton "MemoizedEvent" Memoized.fromEvent Memoized.toEvent p e
            (\_ -> flip speaker2
                (effectfulAudioInterpret' Memoized.fromEvent Memoized.toEvent)
                <<<
                  scene
            )
        , musicButton "MemoizableEvent" Memoizable.fromEvent Memoizable.toEvent
            p
            e
            (\_ -> flip speaker2
                ( effectfulAudioInterpret' Memoizable.fromEvent
                    Memoizable.toEvent
                ) <<<
                scene
            )
        , musicButton "NativeJS" identity identity p e (stressTest_)
        ]

main :: Effect Unit
main = launchAff_ do
  init <- initializeStressTest
  liftEffect do
    b' <- window >>= document >>= body
    for_ (toElement <$> b') \elt -> do
      ffi <- makeFFIDOMSnapshot
      let
        evt = deku elt
          ( subgraph (bang (Tuple unit (InsertOrUpdate unit)))
              (const $ stressTest init (const $ pure unit))
          )
          effectfulDOMInterpret
      _ <- subscribe (evt) \i -> i ffi
      pure unit
