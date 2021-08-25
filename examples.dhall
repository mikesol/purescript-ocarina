let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "halogen"
              , "arrays"
              , "halogen-subscriptions"
              , "identity"
              , "profunctor-lenses"
              , "transformers"
              ]
        }
