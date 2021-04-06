{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pab-sketches"
, dependencies =
  [ "behaviors"
  , "console"
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
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-peano"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
