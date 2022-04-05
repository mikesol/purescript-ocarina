module WAGS.Example.HelloWorld where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, text, text_)
import Deku.DOM as DOM
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Effect (Effect)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, create, subscribe)
import Math (pi, sin)
import WAGS.Control (gain__, sinOsc, speaker2, (:*))
import WAGS.Core (GainInput)
import WAGS.Interpret (close, context, effectfulAudioInterpret, makeFFIAudioSnapshot, writeHead)
import WAGS.Parameter (WriteHead, at_, ovnn, pureOn)
import WAGS.Example.Utils (animationFrameEvent)
import WAGS.Properties (frequency)
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

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \elt -> do
    ffi <- makeFFIDOMSnapshot
    { push, event } <- create
    let
      evt = deku elt
        ( DOM.div_
            [ text_ "Hello world"
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
        )
        effectfulDOMInterpret
    _ <- subscribe evt \i -> i ffi
    push Nothing
