module Ocarina.Example.StressTest where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global as Region
import Control.Monad.ST.Internal as RRef
import Data.Array ((..))
import Data.Filterable (filter, filterMap)
import Data.Int (toNumber)
import Data.Lens (over)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (pi, sin, (%))
import Data.Profunctor (lcmap)
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Domable, bussed)
import Deku.DOM as DOM
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (Event, memoize, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import Ocarina.Clock (WriteHead, fot, writeHead)
import Ocarina.Control (gain, gain_, sinOsc, speaker2)
import Ocarina.Core (Audible, AudioNumeric(..), AudioOnOff(..), _off, _on, _step, opticN, envy)
import Ocarina.Example.Utils (RaiseCancellation)
import Ocarina.Interpret (FFIAudioSnapshot, close, context, effectfulAudioInterpret, makeFFIAudioSnapshot)
import Ocarina.Math (calcSlope)
import Ocarina.Properties as Common
import Ocarina.WebAPI (AudioContext)

last :: Int
last = 100

len :: Number
len = 12.0

lm2 :: Number
lm2 = len - 2.0

scene
  :: forall lock payload
   . WriteHead Event
  -> Audible D2 lock payload
scene wh = envy $ memoize (fot wh (mul pi)) \tr ->
  let
    gso a b c st ed = gain 0.0
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
                    else if olen < ed && olen > (ed - 0.06) then
                      (Just (AudioNumeric (x { n = 0.0, t = _step })))
                    else Nothing
              )
              tr
          )
      )
      [ sinOsc b
          ( Common.frequency <<< (over opticN c) <$>
              ( filter
                  (\(AudioNumeric { o }) -> o % len < ed && o % len > st)
                  tr
              )
              <|> Common.onOff <$>
                ( filterMap
                    ( \(AudioNumeric { o }) ->
                        if o % len < st + 0.5 && o % len > st then
                          (Just (AudioOnOff { x: _on, o }))
                        else if o % len < ed + 0.5 && o % len > ed then
                          (Just (AudioOnOff { x: _off, o }))
                        else Nothing
                    )
                    tr
                )
          )
      ]
  in
      gain_ 1.0 ( map
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
  :: forall event. RRef.STRef Region.Global Int -> AudioContext -> WriteHead event -> event (FFIAudioSnapshot -> Effect Unit)

stressTest
  :: forall lock payload
   . Unit
  -> RaiseCancellation
  -> Domable lock payload
stressTest _ rc = bussed \p -> lcmap (alt $ pure Nothing) \e ->
  let
    musicButton
      :: forall lock0
       . String
      -> (UIAction -> Effect Unit)
      -> Event UIAction
      -> (RRef.STRef Region.Global Int -> AudioContext -> WriteHead Event -> Event (FFIAudioSnapshot -> Effect Unit))
      -> Domable lock0 payload
    musicButton label push event audioEvent = DOM.button
      ( map
          ( \i -> DOM.OnClick := cb
              ( const $
                  maybe
                    ( do
                        ctx <- context
                        rf <- liftST $ RRef.new 0
                        ffi2 <- makeFFIAudioSnapshot ctx
                        let wh = writeHead 0.04 ctx
                        unsub <- subscribe
                          (audioEvent rf ctx (sample_ wh animationFrameEvent))
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
      , musicButton "Event" p e
          ( \rf _ ip -> speaker2 [scene ip] (effectfulAudioInterpret rf)

          )
      , musicButton "NativeJS" p e (stressTest_)
      ]

main :: Effect Unit
main = launchAff_ do
  init <- initializeStressTest
  liftEffect $ runInBody (stressTest init (const $ pure unit))