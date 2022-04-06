module WAGS.Example.HelloWorld where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Indexed.Qualified as Ix
import Control.Plus (empty)
import Data.Either (Either, either)
import Data.Exists (Exists, mkExists)
import Data.Foldable (for_)
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
import FRP.Event (class IsEvent, subscribe)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain__, sinOsc, speaker2, (:*))
import WAGS.Core (GainInput)
import WAGS.Example.Utils (RaiseCancellation, animationFrameEvent)
import WAGS.Imperative (GraphBuilder, InitialGraphBuilderIndex, connect, createGain, createSinOsc, createSpeaker, runGraphBuilder)
import WAGS.Interpret (close, context, effectfulAudioInterpret, makeFFIAudioSnapshot, writeHead)
import WAGS.Parameter (WriteHead, at_, ovnn, pureOn)
import WAGS.Properties (frequency)
import WAGS.WebAPI (AudioContext)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

scene
  :: forall event payload
   . IsEvent event
  => WriteHead event
  -> GainInput D2 () () event payload
scene wh =
  let
    tr = at_ wh (mul pi)
    gso a b c = gain__ a empty
      (sinOsc b (pureOn <|> (frequency <<< (ovnn c) <$> tr)))
  in
    gso 0.1 440.0 (\rad -> 440.0 + (10.0 * sin (2.3 * rad))) :*
      [ gso 0.25 235.0 (\rad -> 235.0 + (10.0 * sin (1.7 * rad)))
      , gso 0.2 337.0 (\rad -> 337.0 + (10.0 * sin rad))
      , gso 0.1 530.0 (\rad -> 530.0 + (19.0 * (5.0 * sin rad)))
      ]

scene'
  :: forall event payload
   . IsEvent event
  => WriteHead event
  -> GraphBuilder event payload InitialGraphBuilderIndex _ Unit
scene' wh = Ix.do
  speaker <- createSpeaker (Proxy :: Proxy "speaker")
  gain0 <- createGain (Proxy :: Proxy "gain0") 0.1 empty
  gain1 <- createGain (Proxy :: Proxy "gain1") 0.25 empty
  gain2 <- createGain (Proxy :: Proxy "gain2") 0.20 empty
  gain3 <- createGain (Proxy :: Proxy "gain3") 0.10 empty
  sinOsc0 <- createSinOsc (Proxy :: Proxy "sinOsc0") 440.0
    (so \rad -> 440.0 + (10.0 * sin (2.3 * rad)))
  sinOsc1 <- createSinOsc (Proxy :: Proxy "sinOsc1") 235.0
    (so \rad -> 235.0 + (10.0 * sin (1.7 * rad)))
  sinOsc2 <- createSinOsc (Proxy :: Proxy "sinOsc2") 337.0
    (so \rad -> 337.0 + (10.0 * sin rad))
  sinOsc3 <- createSinOsc (Proxy :: Proxy "sinOsc3") 530.0
    (so \rad -> 530.0 + (19.0 * (5.0 * sin rad)))
  connect { from: gain0, into: speaker }
  connect { from: gain1, into: speaker }
  connect { from: gain2, into: speaker }
  connect { from: gain3, into: speaker }
  connect { from: sinOsc0, into: gain0 }
  connect { from: sinOsc1, into: gain1 }
  connect { from: sinOsc2, into: gain2 }
  connect { from: sinOsc3, into: gain3 }
  where
  tr = at_ wh (mul pi)
  so f = pureOn <|> (frequency <<< (ovnn f) <$> tr)

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

type Init = Unit

initializeHelloWorld :: Aff Init
initializeHelloWorld = pure unit

helloWorld
  :: forall index event payload
   . IsEvent event
  => Unit
  -> RaiseCancellation
  -> index
  -> Exists (SubgraphF index Unit event payload)
helloWorld _ rc _ = mkExists $ SubgraphF \push -> lcmap (map (either (const Nothing) identity)) \event ->
  DOM.div_
    [ DOM.h1_ [ text_ "Hello world" ]
    , musicButton push event (runGraphBuilder effectfulAudioInterpret <<< scene')
    , musicButton push event (flip speaker2 effectfulAudioInterpret <<< scene)
    ]
  where
  musicButton push event audioEvent =
    DOM.button
      ( map
          ( \i -> DOM.OnClick := cb
              ( const $
                  maybe
                    ( do
                        ctx <- context
                        ffi2 <- makeFFIAudioSnapshot ctx
                        let wh = writeHead 0.04 ctx
                        unsub <- subscribe
                          (audioEvent (sample_ wh animationFrameEvent))
                          ((#) ffi2)
                        rc { unsub, ctx }
                        push $ Just { unsub, ctx }
                    )
                    ( \{ unsub, ctx } -> do
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

main :: Effect Unit
main = launchAff_ do
  init <- initializeHelloWorld
  liftEffect do
    b' <- window >>= document >>= body
    for_ (toElement <$> b') \elt -> do
      ffi <- makeFFIDOMSnapshot
      let
        evt = deku elt
          ( subgraph (pure (Tuple unit (InsertOrUpdate unit)))
              (helloWorld init (const $ pure unit))
          )
          effectfulDOMInterpret
      _ <- subscribe evt \i -> i ffi
      pure unit
