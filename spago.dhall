{ name = "alga"
, dependencies =
  [ "console"
  , "effect"
  , "folds"
  , "lists"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
