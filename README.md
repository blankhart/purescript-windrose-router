# `purescript-servant-routing`

## Introduction

This is a routing library for front-end web applications.  It provides a [Servant](https://github.com/haskell-servant/servant)-style interface to the functionality in [purescript-routing](https://github.com/slamdata/purescript-routing) and [purescript-routing-duplex](https://github.com/natefaubion/purescript-routing-duplex), but abstracting away the parsing and printing of URLs.  

This project is inspired by the Servant-style router used in [Miso](https://github.com/dmjio/miso), a Haskell web framework.  

This library currently uses some combinators from [purescript-servant](https://github.com/f-o-a-m/purescript-servant). The [purescript-trout](https://github.com/owickstrom/purescript-trout) client-server HTTP library uses similar combinators.  The [purescript-kushikatsu](https://github.com/justinwoo/purescript-kushikatsu) router uses a different DSL involving typelevel strings (and exposes a different API to users).  Future releases of this library may be based on a native DSL and adapters for compatibility with these libraries.

Some goals of this project are: (i) to allow easy integration with various web frameworks, (ii) to permit users to define web APIs with nested routes; and (iii) ultimately, to validate web APIs at the type level with custom type errors.  The project is currently in an experimental state and may be significantly restructured in the future.

## Installation

Everything should work by cloning the repo and then:

```bash
yarn install      # also installs bower dependencies
yarn pulp build   # builds project
yarn pulp test    # runs the tests
yarn run:example  # run the example
```

## Usage

Usage starts by defining a typelevel API with named endpoints, as illustrated in `tests/Test/Main.purs`.

```purescript
  type ReadmeApi =
        S "profile" :> CAP "username" String :> VIEW "profile"
    :<|> S "article"
          :> (CAP "id" Int :> VIEW "article_id"
          :<|> S "search" :> QPs ( term :: Required String ) :> VIEW "article_search")
    :<|> S "figures" :> CAPMANY "figures" Int :> VIEW "figures"
```

Each endpoint of the typelevel API must terminate in a `VIEW name` combinator.  The endpoint is uniquely specified by the `name` symbol.  

The library uses the typelevel API to produce the following functions:

* `Servant.Routing.Routable.mkRoutable`.  This is a smart constructor used to turn the user's API into a `Routable api`.  This is just a proxy for a normalized version of the type in which nested alternatives have been removed and all type operators associate to the right.

```purescript
  let api = mkRoutable (RouteProxy :: _ ReadmeApi)
```

* `Servant.Routing.HasRouter.route`.  This runs a user-supplied record of handlers for each named endpoint over a `uri`.  Each endpoint `name` in the API specifies the field label for the corresponding handler. The endpoint's type fully determines the type of the handler.  A handler may accept capture and query parameter arguments.  Each handler must return values of the same type.  For example, in an Elm-like framework, the common return type might be the framework's equivalent of an `Html Msg` (see the example for an variation on this).  A `uri` can be any type with a `Servant.Routing.Location.ToLocation` instance (such as a `String`).

```purescript
  let handlers =
        { profile : \username -> "Profile for " <> username
        , article_id : \(id :: Int) -> "Article #" <> show id
        , article_search: \{ term : Required s } -> "Searched for " <> s
        , figures: \arr -> "Figures: " <> joinWith ", " (show <$> arr)
        }
      match = route api handlers
  assert $ match "/profile/blankhart" === Right "Profile for blankhart"
  assert $ match "/article/34" === Right "Article #34"
  assert $ match "/article/search?term=ptolemy" === Right "Searched for ptolemy"
  assert $ match "/figures/1/2/3/4" === Right "Figures: 1, 2, 3, 4"
```

* `Servant.Routing.HasLinks.allLinksWith`.  This generates a record of safe link generators to the named endpoints.  The link generators may accept capture and query parameter arguments, and by default return a `String` representing the endpoint's URL.  The default can be modified by passing in `allLinksWith` a function of type `Link -> a`, where `Link` is (currently) an alias for `String`.  These functions could be used to generate messages interpreted by a web framework.

```purescript
  let links = allLinksWith identity api
  assert $ links.profile "blankhart" === "/profile/blankhart"
  assert $ links.article_id 34 === "/article/34"
  assert $ links.article_search { term: Required "ptolemy" } === "/article/search?term=ptolemy"
  assert $ links.figures [1, 2, 3, 4] === "/figures/1/2/3/4"
```

These functions should satisfy the property that, for each endpoint in the user's API, running `route` on the link produced by `allLinks` always produces the same value as the corresponding handler.

```purescript
  quickCheck $ \username ->
    route api handlers (links.profile username) === Right (handlers.profile username)
```

## Examples

Working examples may be found in the `test` and `example` directories.  

* The `test` is based on the data type API in the tests for the `purescript-routing` library, in order to facilitate comparison with a well-established router.  That setup is extended with quickcheck-style tests verifying the `route`/`Link` relationship.

* The `example` is based on the router example in the `purescript-hertz` framework.  This framework was chosen because it has a minimal Purescript dependency footprint and came with a well designed illustration of its routing capabilities.
