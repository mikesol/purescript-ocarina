{ name = "purescript-wags"
, dependencies =
  [ "aff-promise"
  , "arraybuffer-types"
  , "behaviors"
  , "console"
  , "convertable-options"
  , "debug"
  , "effect"
  , "event"
  , "free"
  , "heterogeneous"
  , "indexed-monad"
  , "maybe"
  , "ordered-collections"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "sized-vectors"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-peano"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
