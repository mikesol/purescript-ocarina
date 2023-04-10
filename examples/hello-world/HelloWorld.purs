module Ocarina.Example.HelloWorld where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as RRef
import Control.Plus (empty)
import Data.Lens (over)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (pi, sin)
import Data.Profunctor (lcmap)
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Nut, bussed)
import Deku.DOM as DOM
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (Event, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import Ocarina.Clock(WriteHead, fot, writeHead)
import Ocarina.Control (gain, sinOsc, speaker2)
import Ocarina.Core (Audible, bangOn, opticN)
import Ocarina.Example.Utils (RaiseCancellation)
import Ocarina.Interpret (close, context, effectfulAudioInterpret, makeFFIAudioSnapshot)
import Ocarina.Properties (frequency)
import Ocarina.WebAPI (AudioContext)

scene
  :: forall payload
   . WriteHead Event
  -> Array (Audible D2 payload)
scene wh =
  let
    tr = fot wh (mul pi)
    gso a b c = gain a empty
      [ sinOsc b (bangOn <|> (frequency <<< (over opticN c) <$> tr)) ]
  in
    [ gso 0.1 440.0 (\rad -> 440.0 + (10.0 * sin (2.3 * rad)))
    , gso 0.25 235.0 (\rad -> 235.0 + (10.0 * sin (1.7 * rad)))
    , gso 0.2 337.0 (\rad -> 337.0 + (10.0 * sin rad))
    , gso 0.1 530.0 (\rad -> 530.0 + (19.0 * (5.0 * sin rad)))
    ]

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

type Init = Unit

initializeHelloWorld :: Aff Init
initializeHelloWorld = pure unit

helloWorld
  :: forall payload
   . Unit
  -> RaiseCancellation
  -> Nut
helloWorld _ rc = bussed \p -> lcmap (alt (pure Nothing)) \e ->
  let
    musicButton push event audioEvent = DOM.button
      [map
          ( \i -> DOM.OnClick := cb
              ( const $
                  maybe
                    ( do
                        ctx <- context
                        ffi2 <- makeFFIAudioSnapshot ctx
                        rf <- liftST $ RRef.new 0
                        let wh = writeHead 0.04 ctx
                        unsub <- subscribe
                          (audioEvent rf (sample_ wh animationFrameEvent))
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
      ]
      [ text
          (map (maybe "Turn on" (const "Turn off")) event)
      ]
  in
    DOM.div_
      [ DOM.h1_ [ text_ "Hello world" ]
      , musicButton p e
          (\rf i -> speaker2 (scene i) (effectfulAudioInterpret rf))
      ]

main :: Effect Unit
main = launchAff_ do
  init <- initializeHelloWorld
  liftEffect $ runInBody (helloWorld init (const $ pure unit))
