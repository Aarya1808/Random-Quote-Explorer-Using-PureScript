{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "random-quote-explorer"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "affjax-web"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "httpurple"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "prelude"
  , "random"
  , "routing-duplex"
  , "strings"
  , "web-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
