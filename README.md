# purescript-ocarina

A library for web-based interactive audio and audio gaming.

## Documentation

Ocarina's documentation is online [here](https://mikesol.github.io/purescript-ocarina) and the source [in this repository](./examples/docs/).

## Bundling on your site

To see how to bundle this library on your site, please visit the [examples](./examples) directory.

To compile the JS for the hello world example, issue the following command:

```bash
npx spago -x examples.dhall bundle-app \
  --main Ocarina.Example.HelloWorld \
  --to examples/hello-world/index.js
```

Other examples will work the same way, with the directory and module name changing. Then, to access the example, you can run a http server from the directory and navigate to the url, ie `cd examples/hello-world && python -m http.server` and then navigate to localhost:8000.
