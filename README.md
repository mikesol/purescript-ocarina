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

## Rendering audio

The following is the complete hello-world example from the `examples` directory. In this section, we'll decompose it step-by-step to show how audio is rendered.

```purescript
module WAGS.Example.HelloWorld where

import Prelude
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import FRP.Event (subscribe)
import Math (pi, sin)
import WAGS
  ( FFIAudio(..)
  , Frame0
  , Scene
  , SceneI
  , FFIAudio'
  , change
  , create
  , env
  , gain
  , loop
  , run
  , sinOsc
  , speaker
  , start
  , (@>)
  )
import WAGS.Control.Qualified as Ix

scene time =
  let
    rad = pi * time
  in
    speaker
      $ ( (gain 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
            /\ (gain 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
            /\ (gain 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
            /\ (gain 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
            /\ unit
        )

piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
piece =
  Ix.do
    start
    { time } <- env
    create (scene time) $> Right unit
    @> loop
        ( const
            $ Ix.do
                { time } <- env
                ivoid $ change (scene time)
        )

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

myRun :: FFIAudio' -> Effect (Effect Unit)
myRun ffiAudio =
  subscribe
    (run { easingAlgorithm } (FFIAudio ffiAudio) (pure unit) (pure unit) piece)
    (const $ pure unit)

main :: Effect Unit
main = pure unit
```

There are four parts in this example:

1. Import statements.
1. Creation of the audio graph.
1. Creation of the piece.
1. Running the piece.

Let's examine each one.

### Import statements

These are standard PureScript imports. Note that `@>` is an alias for `makeScene`.

### Creation of the audio graph

The audio graph below connects four sine-wave oscillator to a speaker. Each oscillator has its volume controlled by a gain unit.

```purescript
scene time =
  let
    rad = pi * time
  in
    speaker
      $ ( (gain 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
            /\ (gain 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
            /\ (gain 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
            /\ (gain 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
            /\ unit
        )
```

### Creation of the piece

We start the piece by creating the scene from the graph, and then we enter a loop that updates the graph as a function of time.  As the graph's connections never change, meaning that units are never added, removed, or reconnected, the entire piece can be expressed as a single loop.

```purescript
piece :: Scene (SceneI Unit Unit) FFIAudio (Effect Unit) Frame0
piece =
  Ix.do
    start
    { time } <- env
    create (scene time) $> Right unit
    @> loop
        ( const
            $ Ix.do
                { time } <- env
                ivoid $ change (scene time)
        )
```

### Running the piece

The rendering function `run` accepts four parameters and produces output of type `Event Run`, where `Run` is information about the audio graph such as the nodes it contains and the connection between nodes. The actual rendering of audio happens within `run`, so the information contained in the `Run` type is only needed if you want to print information about audio to a console or stream it elsewhere.

The four parameters to run are as follows:

1. Engine info, which for now is just an easing algorithm. The easing algorithm is of type `Cofree ((->) Int) Int` and tells the engine how much lookahead the audio should have in milliseconds. The `(->) Int` is a penalty function, where a positive input is the number of milliseconds left over after rendering (meaning we gave too much headroom) and a negative input is the number of milliseconds by which we missed the deadline (meaning there was not enough headroom). This allows the algorithm to make adjustments if necessary. In this example, we have minimum lookahead of 20 that gets longer if a deadline is missed and trends towards 20 as deadlines are hit.
2. `FFIAudio`. This represents input from the browser like an audio context and buffers. You can see how this is constructed in `examples/hello-world/index.html`.
3. Triggers of type `Event trigger`. This includes ie mouse clicks and MIDI events. In the case of hello world, there are no external triggers, so we use `Unit`.
4. The world of type `Behavior world`. This includes ie the position of a mouse or the ambient temperature. In the case of our `hello-world`, there is no world to measure, so we use `Unit`.

The `main` function at the end is perfunctory and is necessary so that `spago` can bundle it into an `index.js`.

```purescript
easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

myRun :: FFIAudio' -> Effect (Effect Unit)
myRun ffiAudio =
  subscribe
    (run { easingAlgorithm } (FFIAudio ffiAudio) (pure unit) (pure unit) piece)
    (const $ pure unit)

main :: Effect Unit
main = pure unit
```

## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app \
  --main WAGS.Example.HelloWorld \
  --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing.
