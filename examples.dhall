let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "arrays"
              , "arraybuffer"
              , "console"
              , "halogen-subscriptions"
              , "identity"
              , "nonempty"
              , "stringutils"
              , "transformers"
              , "uint"
              , "media-types"
              , "web-file"
              , "exceptions"
              , "nullable"
              , "profunctor-lenses"
              ]
        }
