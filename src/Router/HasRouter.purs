module Windrose.Router.HasRouter
  ( route
  , class HasRouter
  , mkRouter
  , class MatchEndpoint
  , matchEndpoint
  , class MatchTerminal
  , matchTerminal
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record as Record
import Type.Proxy (Proxy(..))
import Windrose.Router.Canonical (Capture, CaptureMany, ECons, ENil, End, Endpoint, Endpoints, NoQuery, Path, Query, Terminal)
import Windrose.Router.Error (RoutingError(..))
import Windrose.Router.Location (class ToLocation, Location(..), toLocation)
import Windrose.Router.QueryPairs (FoldFromQueryPairs, QueryPairs(..), fromQueryPairs)
import Windrose.Router.Routable (Routable)
import Windrose.Router.UrlPiece (class FromUrlPiece, fromUrlPiece)

-- | `HasRouter` instances use the canonical `layout` to build a matcher
-- | from a `Location` to a `page`, which `route` uses to interpret a
-- | `handler`.
-- |
-- | The user specifies endpoints through the typelevel DSL, which are
-- | canonicalized into a `layout`.
-- |
-- | Each routable `layout` corresponds to a user-specified record of type `handler`.
-- |
-- | Each field of the `handler` record is a function accepting capture and
-- | query parameter arguments and producing a value of type `page`.
class HasRouter (layout :: Endpoints) (page :: Type) (handler :: Type) | layout page -> handler where
  mkRouter :: Proxy layout -> handler -> Location -> Maybe page

instance hasRouterNil :: HasRouter ENil page (Record ()) where
  mkRouter _ _ = const Nothing

instance hasRouterCons ::
  ( MatchEndpoint endpoint page viewer
  , HasRouter rest page (Record subhandler)
  , Row.Cons name viewer subhandler handler
  , Row.Lacks name subhandler
  , IsSymbol name
  ) =>
  HasRouter (ECons name endpoint rest) page (Record handler) where
  mkRouter _ handler =
    let
      field = Proxy :: _ name
      here = matchEndpoint (Proxy :: _ endpoint)
      viewer = Record.get field handler
      there = mkRouter (Proxy :: _ rest) (Record.delete field handler)
    in
      -- Stop at the first endpoint that matches.
      \loc -> case here loc of
        Just apply -> Just (apply viewer)
        Nothing -> there loc

-- | `MatchEndpoint` instances match a `Location` against an endpoint.  On a
-- | match they return a function that applies the endpoint's handler to
-- | the values captured from the `Location`, so the matcher is independent
-- | of the handler and is built only once.
class MatchEndpoint (endpoint :: Endpoint) (page :: Type) (handler :: Type) | endpoint page -> handler where
  matchEndpoint :: Proxy endpoint -> Location -> Maybe (handler -> page)

instance matchEndpointPath ::
  ( MatchEndpoint rest page handler
  , IsSymbol s
  ) =>
  MatchEndpoint (Path s rest) page handler where
  matchEndpoint _ =
    let
      segment = reflectSymbol (Proxy :: _ s)
      next = matchEndpoint (Proxy :: _ rest)
    in
      \(Location loc) -> do
        path <- Array.uncons loc.path
        guard $ path.head == segment
        next (Location $ loc { path = path.tail })

instance matchEndpointCapture ::
  ( MatchEndpoint rest page handler
  , FromUrlPiece a
  ) =>
  MatchEndpoint (Capture s a rest) page (a -> handler) where
  matchEndpoint _ =
    let
      next = matchEndpoint (Proxy :: _ rest)
    in
      \(Location loc) -> do
        path <- Array.uncons loc.path
        a <- fromUrlPiece path.head
        apply <- next (Location $ loc { path = path.tail })
        pure \handler -> apply (handler a)

instance matchEndpointCaptureMany ::
  ( MatchTerminal terminal page handler
  , FromUrlPiece a
  ) =>
  MatchEndpoint (CaptureMany s a terminal) page (Array a -> handler) where
  matchEndpoint _ =
    let
      next = matchTerminal (Proxy :: _ terminal)
    in
      \(Location loc) -> do
        as <- traverse fromUrlPiece loc.path
        apply <- next (Location $ loc { path = [] })
        pure \handler -> apply (handler as)

instance matchEndpointEnd ::
  ( MatchTerminal terminal page handler
  ) =>
  MatchEndpoint (End terminal) page handler where
  matchEndpoint _ = matchTerminal (Proxy :: _ terminal)

-- | `MatchTerminal` instances match what remains of a `Location` once the
-- | path pieces of an endpoint have been matched.
class MatchTerminal (terminal :: Terminal) (page :: Type) (handler :: Type) | terminal page -> handler where
  matchTerminal :: Proxy terminal -> Location -> Maybe (handler -> page)

-- | The path is fully matched, allowing for a trailing slash.
atEnd :: Array String -> Boolean
atEnd path = path == [] || path == [ "" ]

instance matchTerminalNoQuery :: MatchTerminal NoQuery page page where
  matchTerminal _ (Location loc) = guard (atEnd loc.path) $> identity

instance matchTerminalQuery ::
  ( RowToList params paramsRL
  , HFoldlWithIndex FoldFromQueryPairs (QueryPairs -> Maybe (Record ())) (Proxy paramsRL) (QueryPairs -> Maybe (Record params))
  ) =>
  MatchTerminal (Query params) page (Record params -> page) where
  matchTerminal _ (Location loc) = do
    guard (atEnd loc.path)
    params <- fromQueryPairs (QueryPairs loc.query)
    pure (_ $ params)

--------------------------------------------------------------------------------
-- Runners
--------------------------------------------------------------------------------

-- | Run a `handler` record over a `uri` to produce a `page`.
-- |
-- | Errors if there is a failure to parse the `uri` into valid
-- | segments via the type's `toLocation` instance, or to match
-- | the `uri` against an endpoint in the canonical API specified
-- | by the `layout`.
-- |
-- | The router is built once the `layout` and `handler` are supplied, so
-- | bind the partial application `route layout handler` once and reuse it
-- | for each `uri` rather than calling `route layout handler uri` each time.
route
  :: forall layout page handler uri
   . HasRouter layout page handler
  => ToLocation uri
  => Routable layout
  -> handler
  -> uri
  -> Either RoutingError page
route _ handler =
  let
    router = mkRouter (Proxy :: _ layout) handler
  in
    \uri -> case toLocation uri of
      Just loc -> note FailedMatch (router loc)
      Nothing -> Left FailedParse
