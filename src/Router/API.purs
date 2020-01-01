module Windrose.Router.API where 

--------------------------------------------------------------------------------
-- Route
--------------------------------------------------------------------------------

foreign import kind Route

data RouteProxy (r :: Route) = RouteProxy

--------------------------------------------------------------------------------
-- Route product
--------------------------------------------------------------------------------

foreign import data RouteProduct :: Route -> Route -> Route
infixr 6 type RouteProduct as :>

--------------------------------------------------------------------------------
-- Route sum
--------------------------------------------------------------------------------

foreign import data RouteSum :: Route -> Route -> Route
infixr 3 type RouteSum as :<|>

--------------------------------------------------------------------------------
-- P (path segment combinator)
--------------------------------------------------------------------------------

foreign import data P :: Symbol -> Route

--------------------------------------------------------------------------------
-- C (capture combinator)
--------------------------------------------------------------------------------

foreign import data C :: Symbol -> Type -> Route

--------------------------------------------------------------------------------
-- M (capture many combinator)
--------------------------------------------------------------------------------

-- | "Capture many" combinator.
foreign import data M :: Symbol -> Type -> Route

--------------------------------------------------------------------------------
-- Q (query parameter combinator)
--------------------------------------------------------------------------------

foreign import data Q :: #Type -> Route

--------------------------------------------------------------------------------
-- V (view combinator)
--------------------------------------------------------------------------------

-- | TODO: Custom type error if V sym :> _ or if Q _ :> a and a /~ VIEW sym
foreign import data V :: Symbol -> Route 

--------------------------------------------------------------------------------
-- NIL (internal combinator)
--------------------------------------------------------------------------------

-- | Users of the library should not use this combinator.
-- TODO: Possibly eliminate and replace with a new kind for canonical paths.
-- Existing setup relies on instance chains and is not kind-safe. 
-- Revisit the structure of the library after PolyKinds.
foreign import data NIL :: Route

