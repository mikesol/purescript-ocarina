module WAGS.Example.Docs.Events.Ex2 where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Filterable (partitionMap)
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute ((:=))
import Deku.Control (text)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.Pursx (makePursx', nut)
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Effect (Effect)
import FRP.Event (class IsEvent, Event, subscribe)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Control (microphone, recorder, singleton, speaker2)
import WAGS.Core (AudioInput)
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page, SingleSubgraphEvent)
import WAGS.Example.Docs.Util (WrapperStates(..), clickCb, mkWrapperEvent)
import WAGS.Interpret (close, context, contextState, effectfulAudioInterpret, getMicrophoneAndCamera, makeFFIAudioSnapshot, mediaRecorderToUrl)
import WAGS.WebAPI (BrowserMicrophone, MediaRecorderCb(..))

px =
  Proxy    :: Proxy   """<section>
  <h2 id="example3">Example 3: A counter</h2>

  <p>In the following example, we emit as many sounds as clicks. So when we click once, we hear one sound, when we click twice, we hear two sounds, when we click thrice, we hear three sounds, and so forth and so on.</p>

  <pre><code>placeholder</code></pre>

  @ex2@

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <p>The important thing to note in this example is the use of <code>on</code>, <code>off</code>, and <code>offOn</code>. They work as follows:</p>

  <table>
    <tr>
      <th>Command</th>
      <th>If currently on</th>
      <th>If currently off</th>
    </tr>
    <tr>
      <td><code>on</code></td>
      <td>Does nothing</td>
      <td>Turns on</td>
    </tr>
    <tr>
      <td><code>off</code></td>
      <td>Turns off</td>
      <td>Does nothing</td>
    </tr>
    <tr>
      <td><code>offOn</code></td>
      <td>Restarts the buffer</td>
      <td>Starts the buffer</td>
    </tr>
  </table>

  <p>We aslo see that on/off events can be scheduled in the future as well using the <code>o</code> parameter of <code>AudioOnOff</code>.</p>
</section>
"""


type RecorderStates = Either String WrapperStates

scene
  :: forall payload
   . BrowserMicrophone
  -> MediaRecorderCb
  -> AudioInput D2 "" () Event payload
scene m cb =
  singleton
    (recorder cb (microphone m))

ex2
  :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> event SingleSubgraphEvent -> Element event payload
ex2 ccb _ ev = makePursx' (Proxy :: _ "@") px
  { ex2: nut
      ( bang (unit /\ Sg.Insert)
          @@ \_ -> mkExists $ SubgraphF \push (event' :: event RecorderStates) ->
            let
              ptn = partitionMap identity event'
              event = mkWrapperEvent ev (_.right ptn)
              aEv = _.left ptn
            in
              D.div_
                [ D.button
                    ( (bang (D.Style := "cursor: pointer;")) <|>
                        ( clickCb ccb (Right >>> push)
                            (_.microphone <$> getMicrophoneAndCamera true false)
                            ( maybe (pure $ pure unit) \mc -> do
                                ctx <- context
                                ffi2 <- makeFFIAudioSnapshot ctx
                                let
                                  audioE = speaker2
                                    ( scene mc
                                        ( MediaRecorderCb $ mediaRecorderToUrl
                                            "audio/ogg; codecs=opus"
                                            (Left >>> push)
                                        )
                                    )
                                    effectfulAudioInterpret
                                unsub <- subscribe
                                  audioE
                                  ( \audio -> audio ffi2
                                  )
                                pure do
                                  unsub
                                  contextState ctx >>= \st -> when (st /= "closed") (close ctx)
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
                , D.div_ [ D.audio ((bang (D.Controls := "true")) <|> (bang (D.Style := "display:none;")) <|> map (\src -> (D.Src := src)) aEv <|> map (const (D.Style := "display:block;")) aEv )  [] ]
                ]
      )
  }
