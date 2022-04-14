module WAGS.Example.Docs.Component where

import Prelude

import Control.Plus (class Plus)
import Deku.Attribute (cb, (:=))
import Deku.Control (text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (class IsEvent)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Example.Docs.AudioUnits.Allpass as Allpass
import WAGS.Example.Docs.AudioUnits.Analyser as Analyser
import WAGS.Example.Docs.AudioUnits.Bandpass as Bandpass
import WAGS.Example.Docs.AudioUnits.Compression as Compression
import WAGS.Example.Docs.AudioUnits.Constant as Constant
import WAGS.Example.Docs.AudioUnits.Convolution as Convolution
import WAGS.Example.Docs.AudioUnits.Delay as Delay
import WAGS.Example.Docs.AudioUnits.TOC as TOC
import WAGS.Example.Docs.Types (CancelCurrentAudio, Page(..), SingleSubgraphEvent, SingleSubgraphPusher)
import WAGS.Example.Docs.Util (scrollToTop)

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
    This section is long and should be read like those passages in the Bible that list who was the child of who: DILIGENTLY AND COPIOUSLY. That said, if you want to skip around, here is a table of contents.
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

  <h2 id="gain">Gain</h2>
  <p>The almighty <a href="https://developer.mozilla.org/en-US/docs/Web/API/GainNode">gain</a> node is your friendly neighborhood volume control. Volume in the web-audio API goes from 0 to 1 whereas we hear logarithmically, so when you're using this, make sure to convert between decibels and gain if you want to work with more intuitive units. The conversion formula is as follows:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts high frequencies using a <code>gain</code> parameter.</p>

  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Wags enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <h2 id="loopbuf">Looping buffer</h2>

  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">looping buffer</a> is buffered audio that loops. The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start and end and optionally its duration.</p>

  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter filter</a> lets lower frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts low frequencies using a <code>gain</code> parameter.</p>

  <h2 id="media">Media element</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaElementAudioSourceNode">media element</a> takes as input a random media element, like audio or a video from the browser. I've used this to filter streams from Spotify through the Web Audio API, for example, but you can use this for any streaming audio or non-streaming audio (basically anything you can cram into an <code>audio</code> or <code>video</code> tag).</p>

  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so. Use it for Web Audio kereoke!</p>

  <h2 id="notch">Notch filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">notch filter</a>, also known as a band-reject filter, attenuates a single frequency range of a source. When you crank up their Q value, the attenuation gets more intense. At the extreme, it sounds like part of the source got sucked into a vacuum, which is not un-interesting!</p>

  <h2 id="panner">Panner</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/PannerNode">panner</a> is absolutely essential if you're making any sort of video game or animation with a camera, for example using WebGL. It is a cheap way for you to distribute your sounds in space without worrying about panning, filtering, volume and lots of other parameters you'd need to tweak if you did this manually. It's my favorite Web Audio node (my favorite <i>today</i>, I rotate them regularly to avoid jealousy, which I probably should do with my own kids but oh well...).</p>

  <h2 id="peaking">Peaking filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">peaking filter</a> is sort of like a notch/bandpass combo. It sounds different than bandpass or notch, and is often a better choice depending on what you're making. The Q works as normal, but the gain either boosts or attenuates the frequency in question if it is positive or negative.</p>

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
components ccb dpage _ _ = px ~~
  { drumroll: nut (text_ "PLACEHOLDER")
  , toc: nut TOC.toc
  , allpass: nut $ Allpass.allpass ccb dpage
  , analyser: nut $ Analyser.analyser ccb dpage
  , bandpass: nut $ Bandpass.bandpass ccb dpage
  , constant: nut $ Constant.constant ccb dpage
  , compression: nut $ Compression.compression ccb dpage
  , convolution: nut $ Convolution.convolution ccb dpage
  , delay: nut $ Delay.delay ccb dpage
  , next: bang (D.OnClick := (cb (const $ dpage Events *> scrollToTop)))
  }