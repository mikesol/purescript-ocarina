module WAGS.Example.Tumult where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, text, text_)
import Deku.Core (Element)
import Deku.DOM as DOM
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, subscribe)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain', gain__, sinOsc, speaker2, (:*))
import WAGS.Core (GainInput, Input)
import WAGS.Example.Utils (animationFrameEvent)
import WAGS.Interpret (close, context, effectfulAudioInterpret, makeFFIAudioSnapshot, writeHead)
import WAGS.Parameter (WriteHead, at_, ovnn, pureOn)
import WAGS.Properties (frequency)
import WAGS.Tumult (tumult)
import WAGS.Tumult.Create.Optionals as Opt
import WAGS.Tumult.Tumult.Make (tumultuously)
import WAGS.WebAPI (AudioContext)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

scene
  :: forall event payload
   . IsEvent event
  => WriteHead event
  -> GainInput D2 (tmlt :: Input) (tmlt :: Input) event payload
scene wh =
  let
    tr = at_ wh (mul pi)
    gso a b c = gain__ a empty
      (sinOsc b (pureOn <|> (frequency <<< (ovnn c) <$> tr)))
  in
    gain__ 0.0 empty
      ( gain' (Proxy :: _ "tmlt") 1.0 empty
          ( gso 0.1 440.0 (\rad -> 440.0 + (10.0 * sin (2.3 * rad))) :*
              [ gso 0.25 235.0 (\rad -> 235.0 + (10.0 * sin (1.7 * rad)))
              , gso 0.2 337.0 (\rad -> 337.0 + (10.0 * sin rad))
              , gso 0.1 530.0 (\rad -> 530.0 + (19.0 * (5.0 * sin rad)))
              ]
          )
      ) :*
      [ tumult
          ( tr <#> \anum ->
              \({ tmlt } :: { tmlt :: Input }) -> tumultuously
                { output: Opt.gain 1.0
                    { hp: Opt.highpass
                        (ovnn (\x -> 2600.0 + 1000.0 * sin x) anum)
                        tmlt
                    }
                }
          )
      ]

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

initializeHelloWorld :: (Unit -> Effect Unit) -> Effect Unit
initializeHelloWorld = (#) unit

helloWorld
  :: forall event payload
   . IsEvent event
  => Unit
  -> Unit
  -> (UIAction -> Effect Unit)
  -> event (Either UIAction UIAction)
  -> Element event payload
helloWorld _ _ push = lcmap (map (either identity identity)) \event -> DOM.div_
  [ DOM.h1_ [ text_ "Hello world" ]
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
                          ( speaker2
                              (scene (sample_ wh animationFrameEvent))
                              effectfulAudioInterpret
                          )
                          ((#) ffi2)
                        push $ Just { unsub: unsub, ctx }
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
  ]

main :: Effect Unit
main = initializeHelloWorld \init -> do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \elt -> do
    ffi <- makeFFIDOMSnapshot
    let
      evt = deku elt
        ( subgraph (pure (Tuple unit (InsertOrUpdate Nothing)))
            (helloWorld init)
        )
        effectfulDOMInterpret
    _ <- subscribe evt \i -> i ffi
    pure unit