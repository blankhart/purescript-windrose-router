# `purescript-windrose-router`

## Introduction

This is a routing library for front-end web applications.  It provides a [Servant](https://github.com/haskell-servant/servant)-style interface to the router matching and handling functionality, but not the hash-based/push-state driver functionality, in [`purescript-routing`](https://github.com/slamdata/purescript-routing).

Like [`purescript-routing-duplex`](https://github.com/natefaubion/purescript-routing-duplex), this library abstracts away the parsing and printing of URLs to ensure at compile-time they are always in sync. But in this library the user specifies the routes through a typelevel DSL, rather than in an intermediate data type representing the routes and a codec interpreting that type.

This project is inspired by the router used in [Miso](https://github.com/dmjio/miso), a Haskell web framework.  The [`purescript-kushikatsu`](https://github.com/justinwoo/purescript-kushikatsu) router has a similar focus with a different DSL and usage pattern.  This library uses type combinators more akin to those in [`purescript-servant`](https://github.com/f-o-a-m/purescript-servant) and [`purescript-trout`](https://github.com/owickstrom/purescript-trout).

## Installation

The project builds with PureScript 0.15 and [spago](https://github.com/purescript/spago), both installed locally through npm. Clone, then:

```bash
npm install                     # installs purs, spago and esbuild
npx spago build                 # builds the library and the example
npx spago test                  # runs the tests
npm run example:halogen:run     # bundles the Halogen example and serves it
```

## Usage

Usage starts by defining a typelevel API with named endpoints.  Route combinators are single letter abbreviations that follow this mnemonic:

* `P` is the *path segment* combinator representing a string.
* `C` is the *capture* combinator for capturing a value.
* `M` is the *capture many* combinator for capturing an array of values.
* `Q` is the *query string* combinator translating a record to a query.
* `V` is the *view* combinator used to name an endpoint.

For example, from `test/Main.purs`:

```purescript
type ReadmeApi =
       P "profile" :> C "username" String :> V "profile"
  :<|> P "article"
        :> ( C "id" Int :> V "article_id"
        :<|> P "search" :> Q (term :: String) :> V "article_search")
  :<|> P "figures" :> M "figures" Int :> V "figures"
```

Each endpoint must terminate in `V name` and must be uniquely specified by `name`.

The library uses the type specified in this way to produce the following functions:

### Routes: `Windrose.Router.Routable.mkRoutable`

This is a smart constructor used to turn the user's API into a `Routable api`.  A value of type `Routable api` is a proxy for a normalized version of the type specified by the user API. In the normalized version, nested alternatives have been removed and all type operators associate to the right.

```purescript
  let api = mkRoutable (RouteProxy :: _ ReadmeApi)
```

### Handlers: `Windrose.Router.HasRouter.route`

The `route` function runs a user-supplied record of handlers for each named endpoint over a `uri`.  A `uri` can be any type with a `ToLocation` instance, such as a `String`.  

Each endpoint `name` in the API specifies the field label for the corresponding handler. The endpoint's type fully determines the type of the handler, including capture and query parameter arguments.  

Each handler in the record must have the same return type.  For example, in an Elm-like framework, the common return type might be the framework's equivalent of an `Html Msg`. Here it is just `String`:

```purescript
  let handlers =
        { profile : \username -> "Profile for " <> username
        , article_id : \(id :: Int) -> "Article #" <> show id
        , article_search: \{ term } -> "Searched for " <> term
        , figures: \arr -> "Figures: " <> joinWith ", " (show <$> arr)
        }
      match = route api handlers
  assert $ match "/profile/blankhart" === Right "Profile for blankhart"
  assert $ match "/article/34" === Right "Article #34"
  assert $ match "/article/search?term=ptolemy" === Right "Searched for ptolemy"
  assert $ match "/figures/1/2/3/4" === Right "Figures: 1, 2, 3, 4"
```

The capture and query parameter arguments can be any type with a `FromUrlPiece` instance.

`route api handlers` builds the router when it receives the handlers, and returns a function that only matches a `uri` against it.  Bind that partial application once, as `match` above, and reuse it.  Calling `route api handlers uri` fully applied rebuilds the router on every call.

Endpoints are tried in the order they appear in the API, and the first one that matches wins.

### Links: `Windrose.Router.HasLinks.allLinks`

The `allLinks` function produces a record of link generators accepting capture and query parameters, and returning a `Link` to the endpoint's URL.  `Link` is currently a type alias for `String`.  

Each endpoint `name` in the API specifies the field label for the corresponding link generator. The endpoint's type fully determines the type of the link generator.  

The default return type can be modified by passing `allLinksWith` a function of type `Link -> a`.  In an Elm-like framework, this could conveniently create `Msg` messages instead of `String` objects.

```purescript
  let links = allLinksWith identity api
  assert $ links.profile "blankhart" === "/profile/blankhart"
  assert $ links.article_id 34 === "/article/34"
  assert $ links.article_search { term: "ptolemy" } === "/article/search?term=ptolemy"
  assert $ links.figures [1, 2, 3, 4] === "/figures/1/2/3/4"
```

The capture and query parameters may be any type with a `ToUrlPiece` instance.

## Laws

These functions should satisfy the property that, for each endpoint in the user's API, running `route` on the link produced by `allLinks` always produces the same value as the corresponding handler.

```purescript
  quickCheck $ \username ->
    match (links.profile username) === Right (handlers.profile username)
```

## Examples

### Tests

The `test` file is based on the data type API in the tests for the [`purescript-routing`](https://github.com/slamdata/purescript-routing) library, to facilitate comparison with a well-established router.  That setup is extended with quickcheck-style tests verifying the `route`/`Link` relationship.

### Halogen

The `examples/halogen` illustration is based on the router example in the [`purescript-halogen`](https://github.com/purescript-halogen/purescript-halogen) framework, specifically the example labeled [`driver-routing`](https://github.com/purescript-halogen/purescript-halogen/tree/master/examples/driver-routing). This example does not have elaborate endpoints.  Run it with `npm run example:halogen:run` and open the address esbuild prints.

To avoid re-running the router on each render, the example uses the router to select a rendering function, which is then cached in the application state.  The framework `render` call just invokes the cached function.

An alternative is to create the router as a separate root component and rely on the framework to limit renders to child components.  The [Real World Halogen project](https://github.com/thomashoneyman/purescript-halogen-realworld/blob/master/src/Component/Router.purs) uses [`purescript-routing-duplex`](https://github.com/natefaubion/purescript-routing-duplex) to parse the route into an intermediate data type, which is then cached in component state.  The `render` call dispatches on the intermediate data type.

This approach is a less natural fit with this library because a `route` call jumps directly from the URL to the result of invoking the corresponding handler.  There is no intermediate data type representing each endpoint.
