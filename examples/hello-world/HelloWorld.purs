module WAGS.Example.HelloWorld where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (for_)
import Data.Lens (over)
import Data.Lens.Iso.Newtype (unto)
import Data.Typelevel.Num (D2)
import Data.Variant (inj)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, text, text_)
import Deku.DOM as DOM
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, Event, create, fold, makeEvent, subscribe)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain__, sinOsc, speaker2, (:*))
import WAGS.Core (GainInput, InitializeGain(..), InitializeSinOsc(..), SinOsc(..))
import WAGS.Interpret (context, effectfulAudioInterpret, makeFFIAudioSnapshot, writeHead)
import WAGS.Parameter (AudioNumeric(..), WriteHead, _apOn, _numeric, at_, propn)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document, requestAnimationFrame)

scene
  :: forall event payload
   . IsEvent event
  => WriteHead event
  -> GainInput D2 () () event payload
scene wh =
  let
    tr = at_ wh (mul pi)
  in
    gain__ (InitializeGain { gain: 0.1 }) empty
      ( sinOsc (InitializeSinOsc { frequency: 440.0 })
          ( pure (SinOsc $ inj (Proxy :: _ "onOff") _apOn) <|>
              ( ( SinOsc <<< inj (Proxy :: _ "frequency") <<< _numeric <<< over
                    (unto AudioNumeric <<< propn)
                    (\rad -> 440.0 + (10.0 * sin (2.3 * rad)))
                ) <$> tr
              )
          )
      ) :*
      [ gain__ (InitializeGain { gain: 0.25 }) empty
          ( sinOsc (InitializeSinOsc { frequency: 235.0 })

              ( pure (SinOsc $ inj (Proxy :: _ "onOff") _apOn) <|>
                  ( ( SinOsc <<< inj (Proxy :: _ "frequency") <<< _numeric <<<
                        over
                          (unto AudioNumeric <<< propn)
                          (\rad -> 235.0 + (10.0 * sin (1.7 * rad)))
                    ) <$> tr
                  )
              )
          )
      , gain__ (InitializeGain { gain: 0.2 }) empty
          ( sinOsc (InitializeSinOsc { frequency: 337.0 })
              ( pure (SinOsc $ inj (Proxy :: _ "onOff") _apOn) <|>
                  ( SinOsc <<< inj (Proxy :: _ "frequency") <<< _numeric <<<
                      over
                        (unto AudioNumeric <<< propn)
                        (\rad -> 337.0 + (10.0 * sin rad)) <$> tr
                  )
              )
          )
      , gain__ (InitializeGain { gain: 0.1 }) empty
          ( sinOsc (InitializeSinOsc { frequency: 530.0 })
              ( pure (SinOsc $ inj (Proxy :: _ "onOff") _apOn) <|>
                  ( SinOsc <<< inj (Proxy :: _ "frequency") <<< _numeric <<<
                      over
                        (unto AudioNumeric <<< propn)
                        (\rad -> 530.0 + (19.0 * (5.0 * sin rad))) <$> tr
                  )
              )
          )
      ]

animationFrameEvent :: Event Unit
animationFrameEvent = makeEvent \k -> do
  w <- window
  running <- Ref.new true
  let
    fx = void $ flip requestAnimationFrame w do
      r' <- Ref.read running
      when r' do
        k unit
        fx
  fx
  pure $ Ref.write false running

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \elt -> do
    ffi <- makeFFIDOMSnapshot
    { push, event } <- create
    let
      switch = fold (\unsub b -> { on: not b.on, unsub }) event
        { on: true, unsub: pure unit }
    let
      evt = deku elt
        ( DOM.div_
            [ text_ "Hello world"
            , DOM.button
                ( map
                    ( \{ on, unsub } -> DOM.OnClick := cb
                        ( const $
                            if on then do
                              unsub
                              push (pure unit)
                            else do
                              ctx <- context
                              ffi2 <- makeFFIAudioSnapshot ctx
                              let wh = writeHead 0.04 ctx
                              unsub2 <- subscribe
                                ( speaker2
                                    (scene (sample_ wh animationFrameEvent))
                                    effectfulAudioInterpret
                                )
                                \i -> do
                                  i ffi2
                              push unsub2
                        )
                    )
                    switch
                )
                [ text
                    (map (_.on >>> if _ then "Turn off" else "Turn on") switch)
                ]
            ]
        )
        effectfulDOMInterpret
    _ <- subscribe evt \i -> i ffi
    push (pure unit)
