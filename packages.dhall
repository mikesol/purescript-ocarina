let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220429/packages.dhall
        sha256:03c682bff56fc8f9d8c495ffcc6f524cbd3c89fe04778f965265c08757de8c9d

let overrides =
      { js-timers =
        { dependencies = [ "refs" ]
        , repo = "https://github.com/mikesol/purescript-js-timers.git"
        , version = "rename-functions"
        }
      }

let additions =
      { event =
        { dependencies =
          [ "console"
          , "effect"
          , "monoid-extras"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.6.5"
        }
      ,  monoid-extras =
        { dependencies =
          [ "prelude"
          ]
        , repo = "https://github.com/mikesol/purescript-monoid-extras.git"
        , version = "v0.0.1"
        }
      ,variant =
        { dependencies =
          [ "assert"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "partial"
          , "prelude"
          , "record"
          , "tuples"
          , "type-equality"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/natefaubion/purescript-variant.git"
        , version = "v8.0.0"
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
        , version = "v8.2.1"
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
      , deku =
        { dependencies =
          [ "arrays"
          , "behaviors"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "event"
          , "exists"
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
          , "unsafe-coerce"
          , "variant"
          , "canvas"
          , "web-dom"
          , "web-events"
          ]
        , repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "v0.3.7"
        }
      , homogeneous =
        { dependencies =
          [ "arrays"
          , "assert"
          , "control"
          , "effect"
          , "enums"
          , "foldable-traversable"
          , "foreign-object"
          , "lists"
          , "maybe"
          , "partial"
          , "prelude"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "variant"
          ]
        , repo = "https://github.com/mikesol/purescript-homogeneous.git"
        , version = "0.15.0"
        }
      , simple-json =
        { dependencies =
          [ "arrays"
          , "assert"
          , "control"
          , "effect"
          , "enums"
          , "foldable-traversable"
          , "foreign-object"
          , "lists"
          , "maybe"
          , "partial"
          , "prelude"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "variant"
          ]
        , repo = "https://github.com/mikesol/purescript-simple-json.git"
        , version = "0.15.0"
        }
      }

in  upstream // overrides // additions
