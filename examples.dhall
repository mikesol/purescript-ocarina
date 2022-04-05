let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples" ++ "/${(env:EXAMPLE as Text) ? ""}" ++ "/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "arrays"
              , "arraybuffer"
              , "deku"
              , "console"
              , "nonempty"
              , "identity"
              , "nonempty"
              , "profunctor"
              , "web-html"
              , "stringutils"
              , "transformers"
              , "uint"
              , "halogen-storybook"
              , "media-types"
              , "web-file"
              , "exceptions"
              , "profunctor-lenses"
              , "free"
              , "math"
              , "refs"
              ]
        }
