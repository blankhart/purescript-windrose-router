module Windrose.Router.API
  ( Route
  , RouteProxy(..)
  , RouteSum
  , RouteProduct
  , type (:<|>)
  , type (:>)
  , P
  , C
  , M
  , Q
  , V
  ) where

-- | Kind for a typelevel DSL of single-page web application route endpoints.
-- |
-- | Routes may be sums (alternatives) and products (path segments, capture and
-- | query parameters).  The algebraic nature of the route specification
-- | is actually used by the process of canonicalizing paths, which relies on
-- | distributivity over addition.
-- |
-- | Every endpoint must end in a named view `V`, optionally preceded by a
-- | query `Q`, and a capture-many `M` must be its last path piece.  The
-- | `Canonicalize` constraint on `mkRoutable` checks these rules and turns the
-- | API into a list of endpoints of kind `Windrose.Router.Canonical.Endpoints`,
-- | whose kinds enforce them from then on.
data Route

-- | A token representing a typelevel API.
data RouteProxy (r :: Route) = RouteProxy

foreign import data RouteSum :: Route -> Route -> Route
foreign import data RouteProduct :: Route -> Route -> Route

-- | Sum combinator.
infixr 3 type RouteSum as :<|>

-- | Product combinator.
infixr 6 type RouteProduct as :>

-- | Path segment combinator.
foreign import data P :: Symbol -> Route

-- | Capture combinator.
foreign import data C :: Symbol -> Type -> Route

-- | Capture-many combinator.  It must be the last path piece of an endpoint.
foreign import data M :: Symbol -> Type -> Route

-- | Query string combinator.  It must come immediately before the view.
foreign import data Q :: Row Type -> Route

-- | View combinator.  It names an endpoint and must end it.
foreign import data V :: Symbol -> Route
