{ name = "alga"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "contravariant"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "folds"
  , "gen"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "spec-quickcheck"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
