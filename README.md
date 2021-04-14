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

Let's look at the type of `SceneT'` first:

1. `env` is the outside environment a scene receives. In the case above, it is `Unit`. Often times, the environment will be a combination of events (ie mouse click events) and behaviors (ie a mouse's position).
2. `audio` contains all the information needed by the engine to render. For web audio, this includes an audio context, buffers and a microphone (amongst other things). For testing, this is just `Unit`.
3. `engine` is the type in which audio is rendered. For actual web audio, this is `Effect Unit`. For testing, this is the `Instruction` type, which is an ADT representation of instructions like `SetFrequency` or `MakeSinOsc`.
4. `proof` is a transactional type that makes sure a `Scene` corresponds to a given moment in time.
5. `m` is the monadic context of the return value from `oneFrameT`. `oneFrame`, used above, extracts the scene from its monadic context using the same pattern as that used in the [`transformers`](https://github.com/purescript/purescript-transformers) library.

Now, let's look at the terms it contains:

1. `nodes` is a map from _pointers_ to _audio units_. Pointers are opaque blobs that allow you to refer to an audio unit, and audio units are things like like sine wave oscillators or highpass filters.
2. `edges` is a map from _pointers_ to _pointers of incoming connections_ in the audio graph.
3. `instructions` is a list of instructions to the audio renderer.
4. `next` can be called with `oneFrame env`, where env is the environment, to get the next `Scene`.


## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app \
  --main WAGS.Example.HelloWorld \
  --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing.
