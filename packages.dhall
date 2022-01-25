let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220110/packages.dhall
        sha256:8dbf71bfc6c7a11043619eebe90ff85f7d884541048aa8cc48eef1ee781cbc0e

let overrides = {=}

let additions =
      { benchotron =
        { dependencies =
          [ "arrays"
          , "console"
          , "datetime"
          , "effect"
          , "exceptions"
          , "exists"
          , "foldable-traversable"
          , "identity"
          , "lcg"
          , "node-fs"
          , "node-readline"
          , "now"
          , "numbers"
          , "profunctor"
          , "quickcheck"
          , "strings"
          , "transformers"
          ]
        , repo = "https://github.com/hdgarrood/purescript-benchotron.git"
        , version = "e64664de1fa0843ca78949f36b31b176fa6b0f84"
        }
      }

in  upstream // overrides // additions
