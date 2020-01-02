module Windrose.Router.API (
  kind Route, 
  RouteProxy(..), 
  RouteSum, 
  RouteProduct, 
  type (:<|>), 
  type (:>), 
  P, C, M, Q, V, 
  NIL
) where 

-- | Kind for a typelevel DSL of single-page web application route endpoints.
-- | 
-- | Routes may be sums (alternatives) and products (path segments, capture and 
-- | query parameters).  The algebraic nature of the route specification
-- | is actually used by the process of canonicalizing paths, which relies on
-- | distributivity over addition.
-- | 
-- | Route endpoints must be named through the view combinator.  This is not
-- | enforced through the kind system, but only through typeclass instance 
-- | resolution and the requirement that the user construct a `Routable api` 
-- | with a `Canonicalize` instance. An alternative design may be possible 
-- | if and when the compiler supports kind polymorphism.
foreign import kind Route

data RouteProxy (r :: Route) = RouteProxy

foreign import data RouteSum :: Route -> Route -> Route
foreign import data RouteProduct :: Route -> Route -> Route

-- | "Sum" combinator.
infixr 3 type RouteSum as :<|>

-- | "Product" combinator.
infixr 6 type RouteProduct as :>

-- | "Path segment" combinator.
foreign import data P :: Symbol -> Route

-- | "Capture" combinator.
foreign import data C :: Symbol -> Type -> Route

-- | "Capture many" combinator.
foreign import data M :: Symbol -> Type -> Route

-- | "Query string" combinator.
foreign import data Q :: #Type -> Route

-- | "View" combinator.
-- TODO: Custom type error if V sym :> _ or if Q _ :> a and a /~ VIEW sym
foreign import data V :: Symbol -> Route 

-- | Users of the library should not use this combinator.
-- TODO: Possibly eliminate and replace with a new kind for canonical paths.
-- Existing setup relies on instance chains and is not kind-safe. 
-- Revisit the structure of the library after PolyKinds.
foreign import data NIL :: Route

-- TODO: Possibly move EMPTY here from Windrose.Router.Routable.
-- Revisit when thinking about possible redesign for polykinds.