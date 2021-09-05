# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.2] - 2021-09-4

### Added

- `stop()` on generators is now sensitive to `AudioParamter` offset.
- all read-only properties of `BrowserAudioBuffer` are now accessible

## [0.4.1] - 2021-09-4

### Fixed

- Assets, environment and behaviors have been merged into just environment.

## [0.4.0] - 2021-09-1

### Added

- `AudioWorkletNode`
- `AnalyserNode`
- type-safe asset cache. **This is a breaking change**! `Scene`, `Frame`, `WAG` and `IxWAG` all take an `asset` type with the current asset cache and fail the build if it is not coherent with the asset being used (ie if a buffer or recorder is used that is not in the cache).

## [0.3.15] - 2021-08-25

### Fixed

- there were missing transitive dependencies that a newer version of `spago` failed the build for. These are now added to the relevant `.dhall` files.

## [0.3.14] - 2021-08-22

### Fixed

- the vector iterator in `fromTemplate` was backwards, leading to backwards-sounding music. This is now fixed!

## [0.3.13] - 2021-08-13

### Added

- hints of a graph can now include be derived from a tuple were the graph is in the functorial position.

## [0.3.12] - 2021-07-18

### Added

- outputs diagnostic info on rendering deadlines.
- passes headroom in seconds to `SceneI`.

## [0.3.11] - 2021-07-15

### Added

- `fromTemplate` now works on row types in addition to sized vectors.

## [0.3.10] - 2021-07-10

### Added

- unnamed units are now possible in create and change commands. When they exist, a name will be chosen by the compiler. The compiler tries to optimize for maximal reuse of audio units to avoid disjunction.

## [0.3.9] - 2021-07-08

### Added

- `startUsingWithHint` allows the quick bootstrapping of audio graphs at the beginning of a scene for instances where a fully-determined graph type is not present but a term or function producing that graph is.

## [0.3.8] - 2021-07-08

### Added

- `startUsing` allows the quick bootstrapping of audio graphs at the beginning of a scene.

## [0.3.7] - 2021-07-07

### Changed

- `SceneI` is now a newtype implementing `Newtype`. This allows it to be used in typeclass instance definitions.

## [0.3.6] - 2021-06-30

### Added

- Microphones, buffers, float arrays, wavetables and recorders are now behaviors instead of static objects. This means that it is possible to make and use new buffers mid-flight! Note that this is a _breaking change_, meaning that all microphones, buffers, float arrays, wavetables and recorders will need to become behaviors. This can be done by prepending existing ones with `pure`.
- The `active` field no longer exists on `SceneI`. Instead, the event is a `Maybe`, with `Nothing` representing an inactive state. This is also a _breaking change_.

## [0.3.5] - 2021-06-28

### Fixed

- Type-level `CreateT` now can handle `AudioParameter OnOff`.

## [0.3.4] - 2021-06-28

### Added

- Make `OnOff` an `AudioParameter`, which allows for more fine-grained starting and stopping in situations with loops.

## [0.3.3] - 2021-06-25

### Added

- `CreateT` and `ChangeT` types now allow one to do type-level operations on audio graphs without constructing terms on the JS level, saving up to a millisecond in rendering time depending on the size and complexity of the graphs being used.

## [0.3.2] - 2021-06-19

### Changed

- Typeclasses for `create` and `change` no longer use `hfoldlWithIndex`.

## [0.3.1] - 2021-06-09

### Added

- By popular demand, adds a looping piecewise function.
- Changes `sysTime` from `Instant` to `Milliseconds` for easier testing in repl.

## [0.3.0] - 2021-06-06

### Added

- Simplifies `change` instructions. Now, instead of writing `{ sinOsc: sinOsc_ 440.0 }` it is possible to write `{ sinOsc: 440.0 }`.
- Speeds up rendering by avoiding unnecessary checks.

### Changed

- The `Optional` files are now split between `Create` and `Change`. This fixes many bugs where a default parameter in a `Create` accidentally modulated a value during a `Change`. Now, the `Change` default is `Nothing`, meaning nothing changes. Furthermore, the underscore syntax (`sinOsc_`) has been removed.

## [0.2.4] - 2021-06-05

### Added

- Audio parameter now has functor, apply, applicative, bind, monad, semigroup and monoid instances.

## [0.2.3] - 2021-06-05

### Added

- Utility functions for working with audio parameters.
- Inlined periodic wave definitions.

## [0.2.2] - 2021-06-05

### Added

- Several functions for working with cofree comonads whose functor varies over time.
- Math functions for working with interpolation.

### Changed

- Eliminates `iwag` and `wag` functions in favor of `icont`.

## [0.2.1] - 2021-06-02

### Added

- An `icont` function for easier continuations using indexed monads.

## [0.2.0] - 2021-06-02

### Changed

- Eliminates custom `do` binding in favor of `Ix.do`.
- Simplifies several signatures and conventions.
- Eliminates reflection of `proof` on the term level.
- Reduces code base size by ~15%.

## [0.1.5] - 2021-05-30

### Added

- A new `forceSet` parameter allows for parameters to be set irrespective of what the previous value was. This eliminates clicks in some situations.

## [0.1.4] - 2021-05-23

### Added

- A new `patch` function allows for the automatic creation, connection, disconnection and destruction of audio units.

### Fixed

- Some bugs in `Interpret.js` caused audio generators to be initialized incorrectly in certain cases when resuming playback. These are fixed.

## [0.1.3] - 2021-05-21

### Added

- Uses `cancelScheduledValues` to cancel future values for an audio parameter.

## [0.1.2] - 2021-05-20

### Added

- New instances for audio parameter make doing math with them easier.

## [0.1.1] - 2021-05-20

### Added

- Makes validation of audio graphs optional in `makeScene`. As audio graphs are already validated as they are built, the extra validation step in `makeScene` mostly catches corner cases. By making validation optional in `makeScene`, projects with complex graphs compile ~100x faster and can opt into validation when needed.

### Changed

- Audio graphs no longer have additional validation by default. Validation is now opt-in.

## [0.1.0] - 2021-05-17

### Added

- Uses extensible records to represent audio graphs.
- Simplifies many function signatures.

### Fixed

- Setting the time of a delay would cause an error due to a misspelled property name. This is now fixed.

## [0.0.4] - 2021-04-29

### Added

- Returns an object from `change`, `changeAt`, `changes` and `change'` that reflects the changed audio unit. Previously, these functions had returned `Unit` (akin to `set` in many libraries), whereas now they return an updated value (akin to `modify`).
- Adds an `asGetter` function to transform any graph into a getter. On `change`, this will get the previous values instead of modifying them.
- Adds a `get` family of functions, `get`, `getAt`, `gets` and `get'` that work exactly like their analogue from the `change` family of functions but act as getters by using `asGetter` internally. Importantly, the `get` family of functions does _not_ increment the change bit.

## [0.0.3] - 2021-04-28

### Added

- Uses strings instead of symbols for setting buffered content. This is a _breaking change_ that moves buffers conceptually closer to other generators. For example, in a sine-wave oscillator, a frequency that changes over time determines what one hears. In a buffer, the buffered audio can be thought of in the same way: it can change over time. That said, changing buffers mid-flight leads to glitchy sound, so the change only takes effect once the buffer is _off_.
- Uses strings instead of symbols for setting the real and imaginary parts of periodic oscillators (see above).

## [0.0.2] - 2021-04-22

### Added

- Pulls in correct bower.json packages for compatibility with projects that do not use spago.

## [0.0.1] - 2021-04-22

### Added

- Adds a bower.json file for publishing to Pursuit.

## [0.0.0] - 2021-04-22

### Added

- Exposes the WebAudio API via a FRP Behavior by using induction on existentially-quantified and linearly-typed indexed cofree comonads.
- A README.
- A CHANGELOG.
- Several tests.
- Several examples.
