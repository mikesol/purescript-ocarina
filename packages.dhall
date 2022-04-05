let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211030/packages.dhall
        sha256:5cd7c5696feea3d3f84505d311348b9e90a76c4ce3684930a0ff29606d2d816c

let overrides =
      { arraybuffer =
        { dependencies =
          [ "arraybuffer-types"
          , "arrays"
          , "effect"
          , "float32"
          , "functions"
          , "gen"
          , "maybe"
          , "nullable"
          , "prelude"
          , "tailrec"
          , "uint"
          , "unfoldable"
          ]
        , repo =
            "https://github.com/purescript-contrib/purescript-arraybuffer.git"
        , version = "v12.0.0"
        }
      }

let additions =
      { typelevel-peano =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "prelude"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/csicar/purescript-typelevel-peano.git"
        , version = "v1.0.1"
        }
      , event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.4.2"
        }
      , everythings-better-with-variants =
        { dependencies =
          [ "control"
          , "foldable-traversable"
          , "invariant"
          , "newtype"
          , "prelude"
          , "variant"
          ]
        , repo =
            "https://github.com/mikesol/purescript-everythings-better-with-variants.git"
        , version = "v0.0.0"
        }
      , typelevel-eval =
        { dependencies =
          [ "effect"
          , "leibniz"
          , "prelude"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mikesol/purescript-typelevel-eval.git"
        , version = "mikesol"
        }
      , behaviors =
        { dependencies =
          [ "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "v8.1.0"
        }
      , row-options =
        { dependencies = [ "homogeneous", "heterogeneous" ]
        , repo = "https://github.com/mikesol/purescript-row-options.git"
        , version = "v0.0.2"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo =
            "https://github.com/natefaubion/purescript-convertable-options.git"
        , version = "v1.0.0"
        }
      , deku = {
        dependencies =  [ "arrays"
  , "behaviors"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "indexed-monad"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "refs"
  , "simple-json"
  , "sized-vectors"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-peano"
  , "unordered-collections"
  , "unsafe-coerce"
  , "variant"
  , "web-dom"
  , "web-events"
  ], repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "v0.1.1"
      },deku-toplevel = {
        dependencies =  [ "arrays"
  , "behaviors"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "indexed-monad"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "refs"
  , "simple-json"
  , "sized-vectors"
  , "transformers"
  , "tuples"
  , "web-html"
  , "typelevel"
  , "typelevel-peano"
  , "unordered-collections"
  , "unsafe-coerce"
  , "variant"
  , "web-dom"
  , "web-events"
  ], repo = "https://github.com/mikesol/purescript-deku-toplevel.git"
        , version = "v0.1.0.1"
      }
      }

in  upstream // overrides // additions
