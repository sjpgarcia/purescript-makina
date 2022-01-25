{ name = "makina"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "st"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
