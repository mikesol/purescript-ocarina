# purescript-wags

PureScript Web Audio Graphs as a Stream.

## Main idea

This library is comprised of two parts.

1. An API for creating streams of web audio graphs.
1. An API for rendering the streams to web audio.

# Examples

There are some other examples to get you started:

- **Atari speaks** ([code](./examples/atari-speaks) | [sound](https://purescript-wags-atari-speaks.surge.sh/))
- **Kitchen sink** ([code](./examples/kitchen-sink) | [sound](https://purescript-wags-kitchen-sink.surge.sh/))
- **The Well-Typed Klavier** ([code](./examples/wtk) | [sound](https://twitter.com/stronglynormal/status/1382221415802408960))

The **Atari speaks** and **Kitchen sink** examples show how to use `purescript-wags` in a [Halogen](https://github.com/purescript-halogen/purescript-halogen) app.

## Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-wags/).

## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
spago -x examples.dhall bundle-app \
  --main WAGS.Example.HelloWorld \
  --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing.
