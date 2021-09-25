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
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "spec"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
