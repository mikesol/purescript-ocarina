let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples/Utils.purs", "examples" ++ "/${(env:EXAMPLE as Text) ? ""}" ++ "/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "arrays"
              , "arraybuffer"
              , "deku"
              , "nonempty"
              , "argonaut-core"
              , "http-methods"
              , "web-dom"
              , "identity"
              , "affjax"
              , "either"
              , "deku-toplevel"
              , "exists"
              , "filterable"
              , "nonempty"
              , "profunctor"
              , "web-html"
              , "stringutils"
              , "uint"
              , "media-types"
              , "web-file"
              , "profunctor-lenses"
              , "free"
              , "math"
              , "refs"
              ]
        }
