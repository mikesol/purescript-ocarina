let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples/Utils.purs", "examples" ++ "/${(env:EXAMPLE as Text) ? ""}" ++ "/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "arrays"
              , "arraybuffer"
              , "avar"
              , "quickcheck"
              , "canvas"
              , "lcg"
              , "qualified-do"
              , "deku"
              , "random"
              , "js-timers"
              , "either"
              , "parallel"
              , "exists"
              , "filterable"
              , "profunctor"
              , "web-html"
              , "uint"
              , "canvas"
              , "media-types"
              , "web-file"
              , "profunctor-lenses"
              , "free"
              , "numbers"
              , "refs"
              ]
        }
