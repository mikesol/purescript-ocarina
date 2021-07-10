# Cheat sheet

Some things you can do in wags:

## Creating a simple graph

```purescript
import WAGS.Create.Optionals as W

graph = W.speaker { myGain: W.gain 0.1 { myOsc: W.sinOsc 440.0 } }
```

## Creating a more complex graph

```purescript
import WAGS.Create.Optionals as W

graph time = W.speaker {
  mainBus: W.gain 1.0 {
     vocals: W.gain 0.5 W.microphone_,
     loop0: W.gain 0.2 (W.loopBuf { loopStart: 0.1, loopEnd: 0.5 } "myBuf0"),
     loop1: W.gain 0.5 (W.highpass 2000.0 (W.loopBuf { loopStart: 0.2, loopEnd: 1.5 } "myBuf1")),
     pad: W.gain 0.12 {
       osc0: W.sinOsc $ 220.0 + sin (pi * time * 3.0) * 10.0,
       osc1: W.sinOsc $ 350.0 + cos (pi * time * 1.3) * 20.0,
       osc2: W.sinOsc $ 490.0 - sin (pi * time * 0.5) * 50.0,
       osc3: W.sinOsc $ 510.0 - cos (pi * time * 20.0) * 100.0,
       osc4: W.gain 0.1 { descant: W.sinOsc 2050.0 }
     }
  }
}
```

Note that the microphone can only appear once and _always_ needs to be called `microphone` in the graph.

## Feedback

Use `ref` to refer to an element elsewhere in the graph, ie to create feedback.

```purescript
import WAGS.Create.Optionals as W

graph time = W.speaker {
  vocals: W.gain 0.5 { microphone: W.microphone, delay: W.delay 0.1 { quiet: W.gain 0.4 { vocals: W.ref } } }
}
```

## Creating a scene

Constructing a graph as shown above is useful when doing live coding using ie [wagsi](https://github.com/mikesol/wagsi) and for shorter snippets, like the [atari-speaks example](./examples/atari-speaks/AtariSpeaks.purs). When working with more complex works (ie [this example](http://wac-wag-3.surge.sh/) whose code lives [here](https://github.com/mikesol/wac-2021/tree/main/example-3/)), you'll often want to construct the types at the type level and use `ipatch` to create the scene.

Here's a basic loop constructed using `ipatch` and `iloop` that creates an upward siren effect.

```purescript
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit as AU
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, RunEngine, SceneI(..))

type MyGraph
  = ( speaker :: AU.TSpeaker /\ { gain :: Unit }
    , gain :: AU.TGain /\ { osc :: Unit }
    , osc :: AU.TSinOsc /\ {}
    )

initialFrame :: IxWAG RunAudio RunEngine Frame0 Unit {} { | MyGraph } Unit
initialFrame = ipatch

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece =
  (const initialFrame)
    @!> iloop \(SceneI { time }) _ -> ichange { gain: 0.2, osc: 440.0 + ((time * 15.0) % 30.0) }
```

## Branching

Large-scale works often need to branch between many different potential outcomes based on input like the current time, mouse clicks or MIDI instruments. Wags allows for type-safe branching using `ibranch`. Check out the `kitchen-sink` example, which makes extensive use of branching.

```purescript
import WAGS.Change (ichange)
import WAGS.Control.Functions (icont)
import WAGS.Control.Functions.Validated (ibranch, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene, WAG)
import WAGS.Graph.AudioUnit as AU
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, RunEngine, SceneI)

type MyGraph1
  = ( speaker :: AU.TSpeaker /\ { gain :: Unit }
    , gain :: AU.TGain /\ { osc :: Unit }
    , osc :: AU.TSinOsc /\ {}
    )

type MyGraph2
  = ( speaker :: AU.TSpeaker /\ { gain :: Unit }
    , gain :: AU.TGain /\ { buf :: Unit }
    , buf :: AU.TLoopBuf /\ {}
    )

initialFrame :: IxWAG RunAudio RunEngine Frame0 Unit {} { | MyGraph1 } Number
initialFrame = ipatch $> 42.0

branch1 ::
  forall proof.
  WAG RunAudio RunEngine proof Unit { | MyGraph1 } Number ->
  Scene (SceneI Unit Unit) RunAudio RunEngine proof Unit
branch1 =
  ibranch \(SceneI e) a ->
    if e.time % 2.0 < 1.0 then
      Right $ ichange { osc: 330.0 } $> a
    else
      Left $ icont branch2 (ipatch $> "hello")

branch2 ::
  forall proof.
  WAG RunAudio RunEngine proof Unit { | MyGraph2 } String ->
  Scene (SceneI Unit Unit) RunAudio RunEngine proof Unit
branch2 =
  ibranch \(SceneI e) a ->
    if e.time % 2.0 > 1.0 then
      Right $ ichange { buf: 10.0 } $> a
    else
      Left $ icont branch1 (ipatch $> 42.0)

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece = const initialFrame @!> branch1
```

## Importing audio files and wave tables

You can stock all of your audio buffers, wavetables, recorders and other assets needed by wags in an `FFIAudio` term that is passed to the `run` function. The example below, from `KitchenSink.purs`, uses Halogen (thus the `H` and `HS`).

```purescript
    { emitter, listener } <- H.liftEffect HS.create
    unsubscribeFromHalogen <- H.subscribe emitter
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    myWave <-
      H.liftEffect
        $ makePeriodicWave audioCtx (0.0 +> -0.1 +> empty) (0.0 +> 0.05 +> empty)
    wicked <- H.liftEffect $ makeFloatArray (makeDistortionCurve 400.0)
    let
      recorder =
        mediaRecorderToUrl
          "audio/ogg; codecs=opus"
          (HS.notify listener <<< HydrateRecording)
    { microphone } <- H.liftAff $ getMicrophoneAndCamera true false
    chimes <- fetchBuffer audioCtx "https://freesound.org/data/previews/353/353194_5121236-hq.mp3"
    shruti <- fetchBuffer audioCtx "https://freesound.org/data/previews/513/513742_153257-hq.mp3"
    let
      ffiAudio =
        (defaultFFIAudio audioCtx unitCache)
          { periodicWaves = O.fromFoldable [ "my-wave" /\ myWave ]
          , buffers = O.fromFoldable [ "my-buffer" /\ chimes, "shruti" /\ shruti ]
          , floatArrays = O.singleton "my-waveshaper" wicked
          , recorders = O.singleton "my-recorder" recorder
          , microphone = toNullable microphone
          }
```

Check out the examples folder, and especially the `kitchen-sink` example, for more examples of how to pre-populate `run` with all the info you'll need.

## Responding to mouse clicks, midi events and sundry

You pass `Event`s and `Behavior`s to `run`. This [example](https://github.com/mikesol/wac-2021/tree/main/example-2), which you can listen to [here](http://wac-wag-2.surge.sh/), uses mouse clicks. The relevant code is here:

```purescript
-- definition of the ADT
data Events
  = StartExample
  | MouseDown

-- call to run using an event of either starting the piece _or_ mouse down
run (pure StartExample <|> (down $> MouseDown)) (pure unit) { easingAlgorithm } (FFIAudio ffiAudio) piece
```