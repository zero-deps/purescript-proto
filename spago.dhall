{ name = "protobuf"
, dependencies =
  [ "arraybuffer-types"
  , "arrays"
  , "console"
  , "effect"
  , "integers"
  , "node-process"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
