module WAGS.Example.Docs.Component where

import Prelude

import Control.Plus (class Plus)
import Deku.Core (Element)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, loopBuf)
import WAGS.Example.Docs.AudioUnits.Allpass as Allpass
import WAGS.Example.Docs.AudioUnits.Analyser as Analyser
import WAGS.Example.Docs.AudioUnits.Bandpass as Bandpass
import WAGS.Example.Docs.AudioUnits.Compression as Compression
import WAGS.Example.Docs.AudioUnits.Constant as Constant
import WAGS.Example.Docs.AudioUnits.Convolution as Convolution
import WAGS.Example.Docs.AudioUnits.Delay as Delay
import WAGS.Example.Docs.AudioUnits.Gain as Gain
import WAGS.Example.Docs.AudioUnits.Highpass as Highpass
import WAGS.Example.Docs.AudioUnits.Highshelf as Highshelf
import WAGS.Example.Docs.AudioUnits.IIRFilter as IIRFilter
import WAGS.Example.Docs.AudioUnits.LoopBuf as LoopBuf
import WAGS.Example.Docs.AudioUnits.Lowpass as Lowpass
import WAGS.Example.Docs.AudioUnits.Lowshelf as Lowshelf
import WAGS.Example.Docs.AudioUnits.Microphone as Microphone
import WAGS.Example.Docs.AudioUnits.Notch as Notch
import WAGS.Example.Docs.AudioUnits.Peaking as Peaking
import WAGS.Example.Docs.AudioUnits.PeriodicOsc as PeriodicOsc
import WAGS.Example.Docs.AudioUnits.PlayBuf as PlayBuf
import WAGS.Example.Docs.AudioUnits.Recorder as Recorder
import WAGS.Example.Docs.AudioUnits.SawtoothOsc as SawtoothOsc
import WAGS.Example.Docs.AudioUnits.SinOsc as SinOsc
import WAGS.Example.Docs.AudioUnits.SquareOsc as SquareOsc
import WAGS.Example.Docs.AudioUnits.StereoPanner as StereoPanner
import WAGS.Example.Docs.AudioUnits.TOC as TOC
import WAGS.Example.Docs.AudioUnits.TriangleOsc as TriangleOsc
import WAGS.Example.Docs.AudioUnits.WaveShaper as WaveShaper
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import WAGS.Example.Docs.Util (audioWrapperSpan, ccassp, ctxAff, mkNext, scrollToTop)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)


  -- todo
  -- <h2 id="media">Media element</h2>
  -- <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaElementAudioSourceNode">media element</a> takes as input a random media element, like audio or a video from the browser. I've used this to filter streams from Spotify through the Web Audio API, for example, but you can use this for any streaming audio or non-streaming audio (basically anything you can cram into an <code>audio</code> or <code>video</code> tag).</p>


  -- todo
  -- <h2 id="panner">Panner</h2>
  -- <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/PannerNode">panner</a> is absolutely essential if you're making any sort of video game or animation with a camera, for example using WebGL. It is a cheap way for you to distribute your sounds in space without worrying about panning, filtering, volume and lots of other parameters you'd need to tweak if you did this manually. It's my favorite Web Audio node (my favorite <i>today</i>, I rotate them regularly to avoid jealousy...).</p>

px = Proxy :: Proxy """<div>
  <h1>Audio Units</h1>

  <h3>There sure are a lot of them!</h3>
  <p>
    This section provides a tour of the web audio nodes provided by the Web Audio API and, by extension, Wags. There are only two omissions:</p>
    <ul>
      <li>Audio Worklet Nodes</li>
      <li>Multi-channel audio</li>
    </ul>
    <p>Both of these will be covered in later sections.</p>

  <p>
    This section is long and should be read like those passages in the Bible that list who was the child of who: DILIGENTLY AND COPIOUSLY. That said, if you want to skip around, here's a table of contents.
  </p>
  ~toc~
  <p>And now, without further ado... (~drumroll~) Here are some audio nodes!</p>

  ~allpass~
  ~analyser~
  ~bandpass~
  ~constant~
  ~compression~
  ~convolution~
  ~delay~
  ~gain~
  ~highpass~
  ~highshelf~
  ~iirFilter~
  ~loopBuf~
  ~lowpass~
  ~lowshelf~
  ~microphone~
  ~notch~
  ~peaking~
  ~periodicOsc~
  ~playBuf~
  ~recorder~
  ~sawtoothOsc~
  ~sinOsc~
  ~squareOsc~
  ~pan~
  ~triangleOsc~
  ~waveShaper~

  <h2>Next steps</h2>
  <p>Phew, that was a lot of audio units! In the next section, we'll make them come alive thanks to the magic of <a ~next~ style="cursor:pointer;">events</a>.</p>
</div>"""

components :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> event SingleSubgraphEvent  -> Element event payload
components cca' dpage ssp ev = px ~~
  { drumroll: nut
      ( audioWrapperSpan "🥁" ev ccb (ctxAff \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/50/50711_179538-lq.mp3")
          \buf -> run2_ $ gain_ 1.0 $ loopBuf buf pureOn
      )
  , toc: nut TOC.toc
  , allpass: nut $ Allpass.allpass ccb dpage ev
  , analyser: nut $ Analyser.analyserEx ccb dpage ev
  , bandpass: nut $ Bandpass.bandpass ccb dpage ev
  , constant: nut $ Constant.constantEx ccb dpage ev
  , compression: nut $ Compression.compression ccb dpage ev
  , convolution: nut $ Convolution.convolution ccb dpage ev
  , delay: nut $ Delay.delay ccb dpage ev
  , gain: nut $ Gain.gain ccb dpage ev
  , highpass: nut $ Highpass.highpass ccb dpage ev
  , highshelf: nut $ Highshelf.highshelf ccb dpage ev
  , iirFilter: nut $ IIRFilter.iirFilterEx ccb dpage ev
  , loopBuf: nut $ LoopBuf.loopBufEx ccb dpage ev
  , lowshelf: nut $ Lowshelf.lowshelf ccb dpage ev
  , lowpass: nut $ Lowpass.lowpass ccb dpage ev
  , notch: nut $ Notch.notch ccb dpage ev
  , playBuf: nut $ PlayBuf.playBufEx ccb dpage ev
  , peaking: nut $ Peaking.peaking ccb dpage ev
  , microphone: nut $ Microphone.microphoneEx ccb dpage ev
  , pan: nut $ StereoPanner.pan ccb dpage ev
  , periodicOsc: nut $ PeriodicOsc.periodic ccb dpage ev
  , recorder: nut $ Recorder.recorderEx ccb dpage ev
  , sawtoothOsc: nut $ SawtoothOsc.sawtooth ccb dpage ev
  , sinOsc: nut $ SinOsc.sine ccb dpage ev
  , squareOsc: nut $ SquareOsc.square ccb dpage ev
  , triangleOsc: nut $ TriangleOsc.triangle ccb dpage ev
  , waveShaper: nut $ WaveShaper.waveShaperEx ccb dpage ev
  , next: mkNext ev cpage
  }
  where
  cpage = dpage Events *> scrollToTop
  ccb = ccassp cca' ssp