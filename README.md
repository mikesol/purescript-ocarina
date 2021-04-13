# purescript-wags

PureScript Web Audio Graphs as a Stream.

## Main idea

This library is comprised of two parts.

1. An API for creating streams of web audio graphs.
1. An API for rendering the streams to web audio.

## Streams

Here is an example of a web audio stream.

```purescript
scene = (start :*> create (speaker (sinOsc 440.0))) @|> freeze

step0 = oneFrame scene unit
step1 = oneFrame step0.next unit
step2 = oneFrame step1.next unit
```

First, we create a scene. The scene can be read as follows:

1. Start the scene.
1. Create a sine wave oscillator at `440.0Hz`.
1. Stay at this value.

Then, we call `oneFrame scene` with an `env` parameter, where `env` is whatever the external environment is. This could be (for example) the time of the audio clock, whether the user is clicking a mouse, MIDI input, or other things that come from an environment.  In the example above, we use a trivial environment of `unit`.

`oneFrame scene` yields a record with the following members:

```purescript
type SceneT' :: forall k. Type -> Type -> Type -> k -> (Type -> Type) -> Type
type SceneT' env audio engine proof m
  = { nodes :: M.Map Int AnAudioUnit
    , edges :: M.Map Int (Set Int)
    , instructions :: Array (audio -> engine)
    , next :: SceneT env audio engine proof m
    }
```

Where:

1. `nodes` is a map from indices to nodes. The example above is translated to `{0: ASpeaker, 1: ASinOsc 440.0}`.
2. `edges` is a map from indices to _incoming_ connections. The sample above is translated to `{0:[1]}`, as the `SinOsc` is going into the speaker.
3. `instructions` is a list of instructions to the Web Audio API for any changes that need to be done. At the first step, the instructions would be `[NewUnit 0 "speaker", NewUnit 1 "sinosc", SetFrequency 1 440.0]`. For the second step (and each subsequent step), the instruction array is empty as nothing changes.
4. `next` represents a new stream, which can be called with `oneFrame env` to get the next `Scene'` record. In our case, we are using `Scene`, which is `SceneT` specialized to the `Thunkable` monad.


## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app \
  --main WAGS.Example.HelloWorld \
  --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing.
