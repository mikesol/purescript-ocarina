# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.3] - 2021-04-22

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
