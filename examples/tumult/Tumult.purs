module WAGS.Example.Tumult where

import Prelude

import Data.Exists (Exists, mkExists)
import Data.Foldable (for_)
import Data.Lens (over)
import Data.Maybe (Maybe(..), maybe)
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
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (Event, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang)
import Math (pi, sin, (%))
import WAGS.Clock (WriteHead, fot, writeHead)
import WAGS.Control (loopBuf, speaker2)
import WAGS.Core (Node, fan)
import WAGS.Example.Utils (RaiseCancellation)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, effectfulAudioInterpret, makeFFIAudioSnapshot)
import WAGS.Parameter (AudioNumeric(..), opticN, bangOn)
import WAGS.Tumult (tumult)
import WAGS.Tumult.Create.Optionals as Opt
import WAGS.Tumult.Tumult.Make (tumultuously)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

-- bring tumult back eventually
-- most of the work is done, but we need to be able to:
-- a) accept arbitrary nodes instead of relying on input
-- b) allow for events on _each_ node, which lowers the amount of diffing

-- scene
--   :: forall lock payload
--    . BrowserAudioBuffer
--   -> WriteHead Event
--   -> Array (Node D2 lock Event payload)
-- scene loopy wh =
--   let
--     tr = fot wh (mul pi)
--   in
--     pure $ fan (loopBuf loopy bangOn) \tmlt ->
--       tumult
--         ( tr <#> \anum@(AudioNumeric { o }) ->
--             let
--               oo = o % 10.0
--               ooo
--                 | oo < 2.0 = tumultuously
--                     { output: Opt.gain 1.0
--                         { lp: Opt.lowpass
--                             ( over opticN (\x -> 1100.0 + 1000.0 * sin x)
--                                 anum
--                             )
--                             tmlt
--                         , osc: Opt.gain 0.03 (Opt.sinOsc 220.0)
--                         }
--                     }
--                 | oo < 4.0 = tumultuously
--                     { output: Opt.gain 1.0
--                         { bp: Opt.bandpass
--                             ( over opticN (\x -> 2000.0 + 1500.0 * sin x)
--                                 anum
--                             )
--                             tmlt
--                         , osc: Opt.gain 0.03 (Opt.sawtoothOsc 330.0)
--                         }
--                     }
--                 | oo < 6.0 = tumultuously
--                     { output: Opt.gain 1.0
--                         { hs: Opt.gain 1.0
--                             { x0: Opt.highshelf
--                                 ( over opticN
--                                     (\x -> 2600.0 + 1000.0 * sin x)
--                                     anum
--                                 )
--                                 tmlt
--                             , x1: Opt.delay 0.04
--                                 (Opt.gain 0.3 { x0: Opt.ref })
--                             }
--                         , osc: Opt.gain 0.03 (Opt.triangleOsc 550.0)
--                         }
--                     }
--                 | oo < 8.0 = tumultuously
--                     { output: Opt.gain 1.0
--                         { ls: Opt.lowshelf
--                             ( over opticN (\x -> 2600.0 + 1000.0 * sin x)
--                                 anum
--                             )
--                             tmlt
--                         , osc: Opt.gain 0.03 (Opt.squareOsc 810.0)
--                         }
--                     }
--                 | otherwise = tumultuously
--                     { output: Opt.gain 1.0
--                         { hp: Opt.highpass
--                             ( over opticN (\x -> 2600.0 + 1000.0 * sin x)
--                                 anum
--                             )
--                             tmlt
--                         , osc: Opt.gain 0.03
--                             ( Opt.periodicOsc
--                                 ( (0.1 +> 0.1 +> 0.02 +> V.empty) /\
--                                     (0.05 +> 0.4 +> 0.1 +> V.empty)
--                                 )
--                                 1020.0
--                             )
--                         }
--                     }
--             in
--               ooo
--         )

-- type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }

-- type Init = BrowserAudioBuffer

-- initializeTumult :: Aff Init
-- initializeTumult = do
--   atar <- liftEffect context >>= flip decodeAudioDataFromUri
--     "https://freesound.org/data/previews/36/36132_321601-hq.mp3"
--   pure atar

-- tumultExample
--   :: forall payload
--    . BrowserAudioBuffer
--   -> RaiseCancellation
--   -> Exists (SubgraphF Event payload)
-- tumultExample loopy rc = mkExists $ SubgraphF \push event ->
--     DOM.div_
--       [ DOM.h1_ [ text_ "Tumult" ]
--       , DOM.button
--           ( map
--               ( \i -> DOM.OnClick := cb
--                   ( const $
--                       maybe
--                         ( do
--                             ctx <- context
--                             ffi2 <- makeFFIAudioSnapshot ctx
--                             let wh = writeHead 0.04 ctx
--                             afe <- animationFrameEvent
--                             unsub <- subscribe
--                               ( speaker2
--                                   ( scene loopy (sample_ wh afe)
--                                   )
--                                   effectfulAudioInterpret
--                               )
--                               ((#) ffi2)
--                             rc $ Just { unsub, ctx }
--                             push $ Just { unsub, ctx }
--                         )
--                         ( \{ unsub, ctx } -> do
--                             rc Nothing
--                             unsub
--                             close ctx
--                             push Nothing
--                         )
--                         i
--                   )
--               )
--               event
--           )
--           [ text
--               (map (maybe "Turn on" (const "Turn off")) event)
--           ]
--       ]

-- main :: Effect Unit
-- main = launchAff_ do
--   init <- initializeTumult
--   liftEffect do
--     b' <- window >>= document >>= body
--     for_ (toElement <$> b') \elt -> do
--       ffi <- makeFFIDOMSnapshot
--       let
--         evt = deku elt
--           ( subgraph (bang (Tuple unit (Insert)))
--               (const $ tumultExample init (const $ pure unit))
--           )
--           effectfulDOMInterpret
--       _ <- subscribe evt \i -> i ffi
--       pure unit