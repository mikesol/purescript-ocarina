module WAGS.Example.HelloWorld where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Plus (empty)
import Data.Exists (Exists, mkExists)
import Data.Foldable (for_)
import Data.Lens (over)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (pi, sin)
import Data.Profunctor (lcmap)
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku1, text, text_)
import Deku.Core (Domable)
import Deku.DOM as DOM
import Deku.Interpret (fullDOMInterpret, makeFFIDOMSnapshot)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Behavior (sample_)
import FRP.Event (Event, bus, keepLatest, memoize, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Clock (WriteHead, fot, writeHead)
import WAGS.Control (gain, sinOsc, speaker2)
import WAGS.Core (Audible, bangOn, opticN)
import WAGS.Example.Utils (RaiseCancellation)
import WAGS.Imperative (InitialGraphBuilder, runGraphBuilder)
import WAGS.Imperative as I
import WAGS.Interpret (close, context, effectfulAudioInterpret, makeFFIAudioSnapshot)
import WAGS.Properties (frequency)
import WAGS.WebAPI (AudioContext)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

scene
  :: forall lock payload
   . WriteHead Event
  -> Array (Audible D2 lock payload)
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

scene'
  :: forall payload
   . WriteHead (Event)
  -> InitialGraphBuilder payload _ Unit
scene' wh = I.do
  speaker <- I.speaker (Proxy :: Proxy "speaker")
  gain0 <- I.gain (Proxy :: Proxy "gain0") 0.1 empty
  gain1 <- I.gain (Proxy :: Proxy "gain1") 0.25 empty
  gain2 <- I.gain (Proxy :: Proxy "gain2") 0.20 empty
  gain3 <- I.gain (Proxy :: Proxy "gain3") 0.10 empty
  sinOsc0 <- I.sinOsc (Proxy :: Proxy "sinOsc0") 440.0
    (so \rad -> 440.0 + (10.0 * sin (2.3 * rad)))
  sinOsc1 <- I.sinOsc (Proxy :: Proxy "sinOsc1") 235.0
    (so \rad -> 235.0 + (10.0 * sin (1.7 * rad)))
  sinOsc2 <- I.sinOsc (Proxy :: Proxy "sinOsc2") 337.0
    (so \rad -> 337.0 + (10.0 * sin rad))
  sinOsc3 <- I.sinOsc (Proxy :: Proxy "sinOsc3") 530.0
    (so \rad -> 530.0 + (19.0 * (5.0 * sin rad)))
  I.connect { from: gain0, into: speaker }
  I.connect { from: gain1, into: speaker }
  I.connect { from: gain2, into: speaker }
  I.connect { from: gain3, into: speaker }
  I.connect { from: sinOsc0, into: gain0 }
  I.connect { from: sinOsc1, into: gain1 }
  I.connect { from: sinOsc2, into: gain2 }
  I.connect { from: sinOsc3, into: gain3 }
  where
  tr = fot wh (mul pi)
  so f = bangOn <|> (frequency <<< (over opticN f) <$> tr)

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

type Init = Unit

initializeHelloWorld :: Aff Init
initializeHelloWorld = pure unit

helloWorld
  :: forall lock payload
   . Unit
  -> RaiseCancellation
  -> Event (Domable Effect lock payload)
helloWorld _ rc = keepLatest $ bus \p -> lcmap (alt (bang Nothing)) \e -> memoize animationFrameEvent \afe ->
  let
    musicButton push event audioEvent = DOM.button
      ( map
          ( \i -> DOM.OnClick := cb
              ( const $
                  maybe
                    ( do
                        ctx <- context
                        ffi2 <- makeFFIAudioSnapshot ctx
                        let wh = writeHead 0.04 ctx
                        unsub <- subscribe
                          (audioEvent (sample_ wh afe))
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
          (map (maybe "Turn on" (const "Turn off")) event)
      ]
  in
    DOM.div_
      [ DOM.h1_ [ text_ "Hello world" ]
      , musicButton p e
          (flip runGraphBuilder effectfulAudioInterpret <<< scene')
      , musicButton p e
          (\i -> speaker2 (scene i) effectfulAudioInterpret)
      ]

main :: Effect Unit
main = launchAff_ do
  init <- initializeHelloWorld
  liftEffect do
    b' <- window >>= document >>= body
    for_ (toElement <$> b') \elt -> do
      ffi <- makeFFIDOMSnapshot
      rf <- Ref.new 0
      let
        evt = deku1 elt
          ( helloWorld init (const $ pure unit))
          (fullDOMInterpret rf)
      _ <- subscribe (evt) \i -> i ffi
      pure unit
