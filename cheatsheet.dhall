let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "cheatsheet/**/*.purs" ]
        , dependencies = conf.dependencies
        }
