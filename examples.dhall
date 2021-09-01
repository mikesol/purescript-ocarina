let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "arrays"
              , "arraybuffer"
              , "halogen-subscriptions"
              , "identity"
              , "stringutils"
              , "transformers"
              , "uint"
              , "media-types"
              , "web-file"
              ]
        }
