module WAGS.Example.Docs.Events.InWags where


import Deku.Core (Element)
import Deku.Pursx (makePursx')
import FRP.Event (Event)
import Type.Proxy (Proxy(..))

px = Proxy :: Proxy
      """<section>
  <h2>Events in Wags</h2>
  <p>Wags follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. As a motivating example, let's look at the definition of <code>sinOsc</code>.</p>

  <pre><code>sinOsc
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels "" C.G_ event payload</code></pre>

  <p>In that defintion, <code>SinOsc</code> is a <code>newtype</code> that is defined as such:</p>

  <pre><code>newtype SinOsc = SinOsc
  (Variant (frequency :: AudioParameter, onOff :: AudioOnOff))</code></pre>

  <p>Informally, the <code>sinOsc</code> function listens to an event that contains one of several possible instructions, including changing the frequency and turning the oscillator on or off. Let's see a few more examples:</p>

    <table>
    <tr>
      <th>Audio Unit</th>
      <th>Newtype name</th>
      <th>Variant</th>
    </tr>
    <tr>
      <td><code>delay</code></td>
      <td><code>Delay</code></td>
      <td><code>Variant (delayTime :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>highshelf</code></td>
      <td><code>Highshelf</code></td>
      <td><code>Variant (frequency :: AudioParameter, gain :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>playBuf</code></td>
      <td><code>PlayBuf</code></td>
      <td><code>Variant (playbackRate :: AudioParameter, buffer :: BrowserAudioBuffer, duration :: Maybe Number, bufferOffset :: Number, onOff :: AudioOnOff)</code></td>
    </tr>
  </table>

  <p>In practice, you'll never need to use newtypes presented above. The <code>WAGS.Properties</code> module has highly-overloaded smart constructors for all these values. For example, to use an <code>Event Number</code> to set the frequency of a <code>sinOsc</code>, you'd write <code>frequency &lt;$&gt; event</code> instead of <code>SinOsc &lt;&lt;&lt; inj (Proxy :: _ "frequency") &lt;&lt;&lt; AudioParameter &lt;&lt;&lt; { o: 0.0, t: _linearRamp, n: _ } &lt;$&gt; event</code>. Of course, you <i>can</i> write the longer format, but life is short and keystrokes are precious! Below is a table showing the varoius smart constructors available and the units to which they apply.</p>


    <table>
    <tr>
      <th>Constructor</th>
      <th>Type</th>
      <th>Receptive Audio Units</th>
    </tr>
    <tr>
      <td><code>bufferOffset</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>delayTime</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>delay</code></li></ul></td>
    </tr>
    <tr>
      <td><code>duration</code></td>
      <td><code>Maybe Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>frequency</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowpass</code></li>
          <li><code>lowshelf</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>gain</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>gain</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowshelf</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>loopEnd</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>loopStart</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>offset</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>onOff</code></td>
      <td><code>AudioOnOff</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
          <li><code>loopBuf</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>playBuf</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>playbackRate</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>loopBuf</code></li><li>playBuf</li></ul></td>
    </tr>
    <tr>
      <td><code>q</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>lowpass</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
  </table>

  <p>You can use this smart-constructor pattern to transform any <code>Event</code> into something that Wags can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Wags:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Wags. Idiomatic Wags projects use functional reactive programming as a way to "steer" web audio, and Wags aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>
</section>"""

inWags :: forall payload. Element Event payload
inWags = makePursx'  (Proxy :: _ "@") px
  {
  }
  -- where
  -- mnx i = mkNext ev (dpage i *> scrollToTop)
  -- ccb = ccassp cca' ssp