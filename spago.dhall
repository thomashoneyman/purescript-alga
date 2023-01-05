{ name = "alga"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "nonempty"
  , "open-folds"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
