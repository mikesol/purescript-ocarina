{ name = "purescript-wags"
, dependencies =
  [ "aff-promise"
  , "behaviors"
  , "console"
  , "convertable-options"
  , "debug"
  , "effect"
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
