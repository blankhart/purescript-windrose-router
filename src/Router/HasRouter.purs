module Windrose.Router.HasRouter (
  route, 
  class HasRouter, 
  mkRouter, 
  Router
) where

import Prelude
import Windrose.Router.API (type (:<|>), type (:>), C, M, NIL, P, Q, RouteProxy(..), V, kind Route)
import Windrose.Router.Error (RoutingError(..))
import Windrose.Router.IsEndpoint (class IsEndpoint)
import Windrose.Router.Location (class ToLocation, Location(..), toLocation)
import Windrose.Router.QueryPairs (QueryPairs(..), fromQueryPairs, FoldFromQueryPairs)
import Windrose.Router.Routable (Routable)
import Windrose.Router.UrlPiece (class FromUrlPiece, fromUrlPiece)

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record as Record
import Type.Data.RowList (RLProxy)

-- | A `Router page` contains the information necessary to execute a handler.
data Router page
  = RAlt (Router page) (Router page)
  | RCapture (String -> Maybe (Router page))
  | RCaptureMany (Array String -> Maybe (Router page))
  | RQueryParam (QueryPairs -> Maybe (Router page))
  | RPathComponent String (Router page)
  | RView page

-- | `HasRouter` instances use the `layout` to build a `Router page`,
-- | which is used internally by `route` to interpret a `handler`. 
-- | 
-- | The user specifies endpoints through the typelevel DSL, which are 
-- | canonicalized and processed into a `layout`.
-- | 
-- | Each routable `layout` corresponds to a user-specified record of type `handler`.
-- |   
-- | Each field of the `handler` record is a function accepting capture and
-- | query parameter arguments and producing a value of type `page`.
class HasRouter (layout :: Route) (page :: Type) (handler :: Type) | layout page -> handler where
  mkRouter :: RouteProxy layout -> handler -> Router page

instance hasRouterRouteSumNil
  ::  ( HasRouter endpoint page viewer
      , IsEndpoint endpoint name
      , Row.Cons name viewer () handler
      , IsSymbol name
      )
  => HasRouter (endpoint :<|> NIL) page (Record handler) where
  mkRouter _ handler = 
    let field = SProxy :: _ name
    in mkRouter (RouteProxy :: RouteProxy endpoint) (Record.get field handler)

else instance hasRouterRouteSumCons
  ::  ( HasRouter endpoint page viewer
      , IsEndpoint endpoint name
      , HasRouter sublayout page (Record subhandler)
      , Row.Cons name viewer subhandler handler
      , Row.Lacks name subhandler
      , IsSymbol name
      )
  => HasRouter (endpoint :<|> sublayout) page (Record handler) where
  mkRouter _ handler = 
    let field = SProxy :: _ name
    in RAlt
        (mkRouter (RouteProxy :: RouteProxy endpoint) (Record.get field handler)) 
        (mkRouter (RouteProxy :: RouteProxy sublayout) (Record.delete field handler))

else instance hasRouterPathComponent
  ::  ( HasRouter sublayout page handler
      , IsSymbol s
      )
  => HasRouter (P s :> sublayout) page handler where
  mkRouter _ handler = RPathComponent (reflectSymbol (SProxy :: SProxy s)) $ 
    mkRouter (RouteProxy :: _ sublayout) handler

else instance hasRouterCapture
  ::  ( HasRouter sublayout page handler
      , IsSymbol s
      , FromUrlPiece a
      )
  => HasRouter (C s a :> sublayout) page (a -> handler) where
  mkRouter _ capture = RCapture $ fromUrlPiece >=> \a -> 
    pure $ mkRouter (RouteProxy :: _ sublayout) (capture a)

else instance hasRouterCaptureMany
  ::  ( HasRouter sublayout page handler
      , IsSymbol s
      , FromUrlPiece a
      )
  => HasRouter (M s a :> sublayout) page (Array a -> handler) where
  mkRouter _ captureMany = RCaptureMany $ traverse fromUrlPiece >=> \arr -> 
    pure $ mkRouter (RouteProxy :: _ sublayout) (captureMany arr)

else instance hasRouterQueryString
  ::  ( HasRouter sublayout page handler
      , RowToList params paramsRL
      , HFoldlWithIndex FoldFromQueryPairs (QueryPairs -> Maybe (Record ())) (RLProxy paramsRL) (QueryPairs -> Maybe (Record params)) 
      ) 
  => HasRouter (Q params :> sublayout) page (Record params -> handler) where 
  mkRouter _ handler = RQueryParam $ \pairs -> do 
    queryRecord <- fromQueryPairs pairs
    pure $ mkRouter (RouteProxy :: _ sublayout) (handler queryRecord)

else instance hasRouterView
  :: ( IsSymbol sym ) => HasRouter (V sym) page page where 
  mkRouter _ page = RView page

--------------------------------------------------------------------------------
-- Routing
--------------------------------------------------------------------------------

-- | Run a computed `Router page` on a `Location` to produce a `page`.
-- | 
-- | Fails if the `Location` does not match any endpoint
-- | in the canonical API used to produce the `Router page`.
routeLoc :: forall page . Location -> Router page -> Maybe page
routeLoc location@(Location loc) r = case r of
  RAlt a b -> 
    routeLoc location a <|> routeLoc location b
  RCapture capture -> do 
    path <- Array.uncons loc.path
    router <- capture path.head
    routeLoc (Location $ loc { path = path.tail }) router
  RCaptureMany captureMany -> do 
    router <- captureMany loc.path
    routeLoc (Location $ loc { path = [] }) router
  RQueryParam interpret -> do
    router <- interpret (QueryPairs loc.query)
    routeLoc (Location $ loc { query = [] }) router 
  RPathComponent segment router -> do 
    path <- Array.uncons loc.path
    guard $ path.head == segment
    routeLoc (Location $ loc { path = path.tail }) router
  RView a ->
    guard (loc.path == [] || loc.path == [""]) $> a

--------------------------------------------------------------------------------
-- Runners
--------------------------------------------------------------------------------

-- | Run a `handler` record over a `Location` to produce a `page`. 
runRouteLoc 
  :: forall layout page handler
   . HasRouter layout page handler
  => Location 
  -> Routable layout 
  -> handler 
  -> Either RoutingError page
runRouteLoc loc nested handler = note FailedMatch $ 
  routeLoc loc $ mkRouter (RouteProxy :: _ layout) handler

-- | Run a `handler` record over a `uri` to produce a `page`. 
-- | 
-- | Errors if there is a failure to parse the `uri` into valid
-- | segments via the type's `toLocation` instance, or to match
-- | the `uri` against an endpoint in the canonical API specified 
-- | by the `layout`.
route 
  :: forall layout page handler uri
   . HasRouter layout page handler
  => ToLocation uri
  => Routable layout
  -> handler
  -> uri
  -> Either RoutingError page
route layout handler uri = case toLocation uri of 
  Just loc -> runRouteLoc loc layout handler
  Nothing -> Left FailedParse