{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "halogen"
    , "profunctor-lenses"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
