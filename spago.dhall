{ name = "makina"
, dependencies =
  [ "arrays"
  , "benchotron"
  , "console"
  , "effect"
  , "fixed-points"
  , "foreign"
  , "gen"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "st"
  , "tailrec"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
