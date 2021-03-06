{ name =
    "windrose-router"
, dependencies =
    [ "aff-coroutines", "effect", "console", "halogen", "heterogeneous", "psci-support", "quickcheck", "strings" ]
, license = 
    "MIT"
, packages =
    ./packages.dhall
, repo = 
    "https://github.com/blankhart/purescript-windrose-router.git"
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, version = 
    "v0.1.1"
}
