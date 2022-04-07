module WAGS.Example.Subgraph where

import Prelude

import Control.Plus (empty)
import Data.Either (either)
import Data.Exists (Exists, mkExists)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Data.Vec ((+>))
import Data.Vec as V
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
import FRP.Event (class IsEvent, subscribe)
import Math (pi, sin, (%))
import Type.Proxy (Proxy(..))
import WAGS.Control (gain', gain__, loopBuf, lowpass, sinOsc, speaker2, (:*))
import WAGS.Core (GainInput, Input, Subgraph(..))
import WAGS.Example.Utils (RaiseCancellation, animationFrameEvent)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, effectfulAudioInterpret, makeFFIAudioSnapshot, writeHead)
import WAGS.Parameter (AudioNumeric(..), WriteHead, at_, ovnn, pureOn)
import WAGS.Subgraph as Wsg
import WAGS.Tumult (tumult)
import WAGS.Tumult.Create.Optionals as Opt
import WAGS.Tumult.Tumult.Make (tumultuously)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

scene
  :: forall event payload
   . IsEvent event
  => BrowserAudioBuffer
  -> WriteHead event
  -> GainInput D2 (toSubg :: Input) (toSubg :: Input) event payload
scene loopy wh =
  let
    tr = at_ wh (mul pi)
  in
    gain__ 0.0 empty
      ( gain' (Proxy :: _ "toSubg") 1.0 empty
          (loopBuf loopy pureOn :* [])
      ) :*
      [ Wsg.subgraph (pure (0 /\ Wsg.InsertOrUpdate unit))
          ( \({ toSubg } :: { toSubg :: Input }) -> Subgraph \i ->
              let
                ooo
                  | otherwise = \e ->
                      lowpass 1100.0
                        (map (ovnn (\x -> 1100.0 + 1000.0 * sin x)) tr)
                        toSubg :* (gain__ 0.03 (sinOsc 220.0 empty))
              -- | i == 1 = \e -> tumultuously
              --           { output: Opt.gain 1.0
              --               { bp: Opt.bandpass
              --                   (ovnn (\x -> 2000.0 + 1500.0 * sin x) anum)
              --                   toSubg
              --               , osc: Opt.gain 0.03 (Opt.sawtoothOsc 330.0)
              --               }
              --           }
              --   | i == 2 = \e -> tumultuously
              --           { output: Opt.gain 1.0
              --               { hs: Opt.gain 1.0
              --                   { x0: Opt.highshelf
              --                       ( ovnn (\x -> 2600.0 + 1000.0 * sin x)
              --                           anum
              --                       )
              --                       toSubg
              --                   , x1: Opt.delay 0.04
              --                       (Opt.gain 0.3 { x0: Opt.ref })
              --                   }
              --               , osc: Opt.gain 0.03 (Opt.triangleOsc 550.0)
              --               }
              --           }
              --   | i == 3 = \e -> tumultuously
              --           { output: Opt.gain 1.0
              --               { ls: Opt.lowshelf
              --                   (ovnn (\x -> 2600.0 + 1000.0 * sin x) anum)
              --                   toSubg
              --               , osc: Opt.gain 0.03 (Opt.squareOsc 810.0)
              --               }
              --           }
              --   | otherwise = \e -> tumultuously
              --           { output: Opt.gain 1.0
              --               { hp: Opt.highpass
              --                   (ovnn (\x -> 2600.0 + 1000.0 * sin x) anum)
              --                   toSubg
              --               , osc: Opt.gain 0.03
              --                   ( Opt.periodicOsc
              --                       ( (0.1 +> 0.1 +> 0.02 +> V.empty) /\
              --                           (0.05 +> 0.4 +> 0.1 +> V.empty)
              --                       )
              --                       1020.0
              --                   )
              --               }
              --           }
              in
                ooo
          )
      ]

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

type Init = BrowserAudioBuffer

initializeTumult :: Aff Init
initializeTumult = do
  atar <- liftEffect context >>= flip decodeAudioDataFromUri
    "https://freesound.org/data/previews/36/36132_321601-hq.mp3"
  pure atar

tumultExample
  :: forall event payload
   . IsEvent event
  => BrowserAudioBuffer
  -> RaiseCancellation
  -> Exists (SubgraphF Unit event payload)
tumultExample loopy rc = mkExists $ SubgraphF \push -> lcmap
  (map (either (const Nothing) identity))
  \event ->
    DOM.div_
      [ DOM.h1_ [ text_ "Tumult" ]
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
                                  (scene loopy (sample_ wh animationFrameEvent))
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
  init <- initializeTumult
  liftEffect do
    b' <- window >>= document >>= body
    for_ (toElement <$> b') \elt -> do
      ffi <- makeFFIDOMSnapshot
      let
        evt = deku elt
          ( Sg.subgraph (pure (Tuple unit (Sg.InsertOrUpdate unit)))
              (const $ tumultExample init (const $ pure unit))
          )
          effectfulDOMInterpret
      _ <- subscribe evt \i -> i ffi
      pure unit