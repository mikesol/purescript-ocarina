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
import WAGS.Example.Docs.AudioUnits.Notch as Notch
import WAGS.Example.Docs.AudioUnits.Peaking as Peaking
import WAGS.Example.Docs.AudioUnits.TOC as TOC
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import WAGS.Example.Docs.Util (audioWrapperSpan, ccassp, ctxAff, mkNext, scrollToTop)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Parameter (pureOn)
import WAGS.Run (run2_)

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

  <h2 id="media">Media element</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaElementAudioSourceNode">media element</a> takes as input a random media element, like audio or a video from the browser. I've used this to filter streams from Spotify through the Web Audio API, for example, but you can use this for any streaming audio or non-streaming audio (basically anything you can cram into an <code>audio</code> or <code>video</code> tag).</p>

  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so. Use it for Web Audio kereoke!</p>

  ~notch~

  <h2 id="panner">Panner</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/PannerNode">panner</a> is absolutely essential if you're making any sort of video game or animation with a camera, for example using WebGL. It is a cheap way for you to distribute your sounds in space without worrying about panning, filtering, volume and lots of other parameters you'd need to tweak if you did this manually. It's my favorite Web Audio node (my favorite <i>today</i>, I rotate them regularly to avoid jealousy, which I probably should do with my own kids but oh well...).</p>

  ~peaking~

  <h2 id="playbuf">Playing a buffer</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">Playback from a buffer</a> is one of the bread-and-butter operations in Web Audio (or any audio). The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start time and optionally its duration.</p>

  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app 🎙️.</p>

  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>

  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>

  <h2 id="square">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">square wave oscillator</a> plays back a square wave at a given frequency.</p>

  <h2 id="pan">Stereo panner</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane..</p>

  <h2 id="triangle">Triangle wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">triangle wave oscillator</a> plays back a triangle wave at a given frequency.</p>

  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <h2>Next steps</h2>
  <p>Phew, that was a lot of audio units! In the next section, we'll make them come alive thanks to the magic of <a ~next~ style="cursor:pointer;">events</a>.</p>
</div>"""

components :: forall event payload. IsEvent event => Plus event => CancelCurrentAudio -> (Page -> Effect Unit) -> SingleSubgraphPusher -> event SingleSubgraphEvent  -> Element event payload
components cca' dpage ssp ev = px ~~
  { drumroll: nut
      ( audioWrapperSpan "🥁" ev ccb (ctxAff >>= \ctx -> decodeAudioDataFromUri ctx "https://freesound.org/data/previews/50/50711_179538-lq.mp3")
          \buf -> run2_
            [ gain_ 1.0 [loopBuf buf pureOn] ]
      )
  , toc: nut TOC.toc
  , allpass: nut $ Allpass.allpass ccb dpage ev
  , analyser: nut $ Analyser.analyser ccb dpage ev
  , bandpass: nut $ Bandpass.bandpass ccb dpage ev
  , constant: nut $ Constant.constantEx ccb dpage ev
  , compression: nut $ Compression.compression ccb dpage ev
  , convolution: nut $ Convolution.convolution ccb dpage ev
  , delay: nut $ Delay.delay ccb dpage ev
  , gain: nut $ Gain.gain ccb dpage ev
  , highpass: nut $ Highpass.highpass ccb dpage ev
  , highshelf: nut $ Highshelf.highshelf ccb dpage ev
  , loopBuf: nut $ LoopBuf.loopBufEx ccb dpage ev
  , lowshelf: nut $ Lowshelf.lowshelf ccb dpage ev
  , lowpass: nut $ Lowpass.lowpass ccb dpage ev
  , notch: nut $ Notch.notch ccb dpage ev
  , iirFilter: nut $ IIRFilter.iirFilterEx ccb dpage ev
  , peaking: nut $ Peaking.peaking ccb dpage ev
  , next: mkNext ev cpage
  }
  where
  cpage = dpage Events *> scrollToTop
  ccb = ccassp cca' ssp