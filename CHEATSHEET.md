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
     vocals: W.gain 0.5 { microphone: W.microphone },
     loop0: W.gain 0.2 { buf0: W.loopBuf { loopStart: 0.1, loopEnd 0.5 } "myBuf0" },
     loop1: W.gain 0.5 { hpf1: W.highpass 2000.0 { buf1: W.loopBuf { loopStart: 0.2, loopEnd 1.5 } "myBuf1" } },
     pad: W.gain 0.12 {
       osc0: W.sinOsc 220.0 + sin (pi * time * 3.0) * 10.0,
       osc1: W.sinOsc 350.0 + cos (pi * time * 1.3) * 20.0,
       osc2: W.sinOsc 490.0 - sin (pi * time * 0.5) * 50.0,
       osc3: W.sinOsc 510.0 - cos (pi * time * 20.0) * 100.0,
       osc4: W.gain 0.1 { descant: W.sinOsc 2050.0 }
     },
  }
}
```

Note that the microphone can only appear once and _always_ needs to be called `microphone` in the graph.

## Feedback

Use `ref` to refer to an element elsewhere in the graph, ie to create feedback.

```purescript
import WAGS.Create.Optionals as W

graph time = W.speaker {
  vocals: W.gain 0.5 { microphone: W.microphone, delay: W.delay 0.1 { quiet: W.gain 0.4 { vocals: ref } } }
}
```

## Creating a scene

Constructing a graph as shown above is useful when doing live coding using ie [wagsi](https://github.com/mikesol/wagsi) and for shorter snippets, like the atari-speaks example. When working with more complex works (ie [this example](http://wac-wag-3.surge.sh/) whose code lives [here](https://github.com/mikesol/wac-2021/tree/main/example-3/)), you'll often want to construct the types at the type level and use `ipatch` to create the scene.

Here's a basic loop constructed using `ipatch` and `iloop` that creates an upward siren effect.

```purescript
import WAGS.Control.Functions.Validated (ipatch, iloop, (@!>))

type MyGraph = 
  { speaker :: TSpeaker /\ { gain :: Unit }
  , gain :: TGain /\ { osc :: Unit }
  , osc :: TSinOsc /\ {}  
  }

initialFrame :: IxFrame RunAudio RunEngine Frame0 Unit {} { | MyGraph } Unit
initialFrame = istart *> ipatch

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece = (const initialFrame) @!> iloop \{ time } _ -> ichange { gain: 0.2, osc: 440.0 + (e.time * 15.0 % 30.0) }
```

## Branching

Large-scale works often need to branch between many different potential outcomes based on input like the current time, mouse clicks or MIDI instruments. Wags allows for type-safe branching using `ibranch`. Check out the `kitchen-sink` example, which makes extensive use of branching.

```purescript
-- todo, add branching example
```

## Importing audio files and wave tables

You can stock all of your audio buffers, wavetables, recorders and other assets needed by wags in an `FFIAudio` term that is passed to the `run` function.

```purescript
-- todo: add
```

Check out the examples folder, and especially the `kitchen-sink` example, for more examples of how to pre-populate `run` with all the info you'll need.

## Responding to mouse clicks, midi events and sundry.

You pass `Event`s and `Behavior`s to `run`. This example, which you can listen to here, uses mouse clicks. The relevant code is here:

```purescript
-- todo: add
```