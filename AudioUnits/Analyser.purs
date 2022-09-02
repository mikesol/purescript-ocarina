module Ocarina.Example.Docs.AudioUnits.Analyser where

import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as RRef
import Control.Plus (class Plus)
import Data.ArrayBuffer.Typed (toArray)
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Filterable (partitionMap)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2, D5, D8, d0, d1, d2, d3, d4, d5, d6, d7)
import Data.UInt (toInt)
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Deku.Attribute ((:=))
import Deku.Control (text)
import Deku.Core (Domable, bussed)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Effect (Effect, foreachE)
import Effect.Ref (modify_, new, read, write)
import FRP.Event (class IsEvent, Event, bus, create, sampleOn_, subscribe)
import FRP.Event.Animate (animationFrameEvent)
import Ocarina.Control (analyser_, loopBuf, speaker2)
import Ocarina.Core (Po2(..))
import Ocarina.Core (bangOn)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import Ocarina.Example.Docs.Util (WrapperStates(..), clickCb, mkWrapperEvent)
import Ocarina.Interpret (close, context, contextState, bracketCtx, decodeAudioDataFromUri, effectfulAudioInterpret, getByteFrequencyData, makeFFIAudioSnapshot)
import Ocarina.WebAPI (AnalyserNodeCb(..), BrowserAudioBuffer)
import Type.Proxy (Proxy(..))

px =
  Proxy    :: Proxy         """<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Ocarina provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Ocarina as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
"""

type AnalyserLogic = Vec D8 (Vec D5 Boolean)

type AnalyserStates = Either AnalyserLogic WrapperStates

style0 :: String
style0 = "background-color: rgb(10,100,0);"

style1 :: String
style1 = "background-color: rgb(10,130,10);"

style2 :: String
style2 = "background-color: rgb(80,90,10);"

style3 :: String
style3 = "background-color: rgb(130,60,10);"

style4 :: String
style4 = "background-color: rgb(150,30,10);"

scene atar cb =   analyser_ { cb, fftSize: TTT8 } [loopBuf atar bangOn]

b0 :: Number
b0 = 1.0 / 40.0

b1 :: Number
b1 = 3.0 / 40.0

b2 :: Number
b2 = 7.0 / 40.0

b3 :: Number
b3 = 10.0 / 40.0

b4 :: Number
b4 = 15.0 / 40.0

bgWhite :: String
bgWhite = "background-color: rgb(255,255,255,0.0);"

stys = V.fill (\_ -> style4 +> style3 +> style2 +> style1 +> style0 +> V.empty) :: Vec D8 (Vec D5 String)
mkSt i0 i1 e = map (\v -> if V.index (V.index v i0) i1 then D.Style := V.index (V.index stys i0) i1 else D.Style := bgWhite) e

analyserEx
  :: forall lock payload. CancelCurrentAudio -> (Page -> Effect Unit) -> Event SingleSubgraphEvent -> Domable lock payload
analyserEx ccb _ ev = px ~~
  { analyser: nut
      (bussed \push (event' :: Event AnalyserStates) ->
            let
              ptn = partitionMap identity event'
              event = mkWrapperEvent ev  (_.right ptn)
              aEv = _.left ptn
            in
              D.div_
                [ D.button
                    ( (pure (D.Style := "cursor: pointer;")) <|>
                        ( clickCb ccb (Right >>> push)
                            (\ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/320/320873_527080-hq.mp3")
                            ( \ctx atar -> do
                                analyserE <- new Nothing
                                ffi2 <- makeFFIAudioSnapshot ctx
                                rf <- liftST (RRef.new 0)
                                let
                                  audioE = speaker2
                                    [scene atar
                                        ( AnalyserNodeCb
                                            ( \a -> do
                                                write (Just a) analyserE
                                                pure (write Nothing analyserE)
                                            )
                                        )
                                    ]
                                    (effectfulAudioInterpret rf)
                                unsub <- subscribe
                                  ( map Right audioE <|> map Left animationFrameEvent
                                  )
                                  ( \analyserOrAudio -> case analyserOrAudio of
                                      Right audio -> audio ffi2
                                      Left _ -> do
                                        analyser <- read analyserE
                                        for_ analyser \a -> do
                                          frequencyData <-
                                            getByteFrequencyData a
                                          arr <- toArray frequencyData
                                          r0 <- new 0
                                          r1 <- new 0
                                          r2 <- new 0
                                          r3 <- new 0
                                          r4 <- new 0
                                          r5 <- new 0
                                          r6 <- new 0
                                          r7 <- new 0
                                          tref <- new 0
                                          cref <- new 0
                                          let
                                            gcref x
                                              | x < 32 = r0
                                              | x < 64 = r1
                                              | x < 96 = r2
                                              | x < 128 = r3
                                              | x < 168 = r4
                                              | x < 160 = r5
                                              | x < 224 = r6
                                              | otherwise = r7
                                          foreachE arr \i' -> do
                                            let i = toInt i'
                                            nref <- read cref
                                            modify_ (add i) tref
                                            modify_ (add i) (gcref nref)
                                            modify_ (add 1) cref
                                          ov <- (r0 +> r1 +> r2 +> r3 +> r4 +> r5 +> r6 +> r7 +> V.empty) # traverse \i -> do
                                            x <- toNumber <$> (read i)
                                            v <- div x <$> (toNumber <$> read tref)
                                            pure ((v > b4) +> (v > b3) +> (v > b2) +> (v > b1) +> (v > b0) +> V.empty)
                                          push $ Left ov
                                  )
                                pure do
                                  unsub
                                  contextState ctx >>= \st -> when (st /= "closed") (close ctx)
                                  push $ Left (V.fill (const (V.fill (const false))))
                            )
                            ev
                            event
                        )
                    )
                    [ text
                        ( map
                            ( case _ of
                                Stopped -> "Turn on"
                                Loading -> "Loading..."
                                Playing _ -> "Turn off"
                            )
                            event
                        )
                    ]
                -- grid-auto-rows: 20px;
                , D.div (pure $ D.Style := "display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")
                    [ D.div ((pure $ D.Style := bgWhite) <|> (mkSt d0 d0 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d1 d0 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d2 d0 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d3 d0 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d4 d0 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d5 d0 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d6 d0 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d7 d0 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d0 d1 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d1 d1 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d2 d1 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d3 d1 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d4 d1 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d5 d1 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d6 d1 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d7 d1 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d0 d2 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d1 d2 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d2 d2 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d3 d2 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d4 d2 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d5 d2 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d6 d2 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d7 d2 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d0 d3 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d1 d3 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d2 d3 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d3 d3 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d4 d3 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d5 d3 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d6 d3 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d7 d3 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d0 d4 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d1 d4 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d2 d4 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d3 d4 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d4 d4 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d5 d4 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d6 d4 aEv)) []
                    , D.div ((pure $ D.Style := bgWhite) <|> (mkSt d7 d4 aEv)) []
                    ]
                ]
      )
  }