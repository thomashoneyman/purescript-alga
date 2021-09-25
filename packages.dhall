let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210825/packages.dhall sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f

in  upstream
  with folds = 
    { repo = "https://github.com/purescript-open-community/purescript-folds.git"
    , version = "v6.3.0"
    , dependencies = [ "control", "profunctor", "ordered-collections" ]
    }
