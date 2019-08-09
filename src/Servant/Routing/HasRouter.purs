module Servant.Routing.HasRouter where

import Prelude
import Servant.API
import Servant.Routing.API
import Servant.Routing.Error (RoutingError(..))
import Servant.Routing.IsEndpoint (class IsEndpoint)
import Servant.Routing.Location (class ToLocation, Location(..), toLocation)
import Servant.Routing.QueryPairs (FoldQueryPairs(..), QueryPairs(..))
import Servant.Routing.Routable (Routable)

import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList (kind RowList, class RowToList)
import Record as Record
import Type.Data.RowList (RLProxy(..))

--------------------------------------------------------------------------------
-- A 'Router' contains the information necessary to execute a handler.
--------------------------------------------------------------------------------

data Router page
  = RAlt (Router page) (Router page)
  | RCapture (String -> Maybe (Router page))
  | RQueryParam (QueryPairs -> Maybe (Router page))
  | RPathComponent String (Router page)
  | RView page

--------------------------------------------------------------------------------
-- 'HasRouter' builds a 'Router'.
--------------------------------------------------------------------------------

class HasRouter (layout :: Route) (page :: Type) (handler :: Type) | layout page -> handler where
  mkRouter :: RouteProxy layout -> handler -> Router page

-- | Path Alternative
instance hasRouterPathAltNil
  ::  ( HasRouter endpoint page viewer
      , IsEndpoint endpoint name
      , Row.Cons name viewer () handler
      , IsSymbol name
      )
  => HasRouter (endpoint :<|> NIL) page (Record handler) where
  mkRouter _ handler = 
    let field = SProxy :: SProxy name
    in mkRouter (RouteProxy :: RouteProxy endpoint) (Record.get field handler)

else instance hasRouterPathAltCons
  ::  ( HasRouter endpoint page viewer
      , IsEndpoint endpoint name
      , HasRouter sublayout page (Record subhandler)
      , Row.Cons name viewer subhandler handler
      , Row.Lacks name subhandler
      , IsSymbol name
      )
  => HasRouter (endpoint :<|> sublayout) page (Record handler) where
  mkRouter _ handler = 
    let field = SProxy :: SProxy name
    in RAlt
        (mkRouter (RouteProxy :: RouteProxy endpoint) (Record.get field handler)) 
        (mkRouter (RouteProxy :: RouteProxy sublayout) (Record.delete field handler))

-- | Path Component
else instance hasRouterPathComponent
  ::  ( HasRouter sublayout page handler
      , IsSymbol s
      )
  => HasRouter (S s :> sublayout) page handler where
  mkRouter _ handler = RPathComponent (reflectSymbol $ SProxy :: SProxy s) $ 
    mkRouter (RouteProxy :: RouteProxy sublayout) handler

-- | Capture
else instance hasRouterCapture
  ::  ( HasRouter sublayout page handler
      , IsSymbol s
      , FromCapture a
      )
  => HasRouter (CAP s a :> sublayout) page (a -> handler) where
  mkRouter _ capture = RCapture $ fromCapture >=> \a -> 
    pure $ mkRouter (RouteProxy :: RouteProxy sublayout) (capture a)

-- | QueryParam 
else instance hasRouterQueryParam 
  ::  ( HasRouter sublayout page handler
      , RowToList params paramsRL
      , HFoldlWithIndex FoldQueryPairs (QueryPairs -> Maybe (Record ())) (RLProxy paramsRL) (QueryPairs -> Maybe (Record params)) 
      ) 
  => HasRouter (QPs params :> sublayout) page (Record params -> handler) where 
  mkRouter _ handler = RQueryParam $ \pairs -> do 
    let empty = (const (Just {}) :: QueryPairs -> Maybe (Record ()))
    queryRecord <- hfoldlWithIndex FoldQueryPairs empty (RLProxy :: RLProxy paramsRL) $ pairs
    pure $ mkRouter (RouteProxy :: RouteProxy sublayout) (handler queryRecord)

-- | VIEW
else instance hasRouterView
  :: ( IsSymbol sym ) => HasRouter (VIEW sym view) page page where 
  mkRouter _ page = RView page

--------------------------------------------------------------------------------
-- Routing
--------------------------------------------------------------------------------

-- | Run a computed 'Router page' on a 'Location' so as to produce a 'page'.
-- | Fail if the 'Location' does not match the 'Routable' used to produce the 
-- | 'Router page'.
routeLoc :: forall page . Location -> Router page -> Maybe page
routeLoc location@(Location loc) r = case r of
  RAlt a b -> 
    routeLoc location a <|> routeLoc location b
  RCapture capture -> do 
    path <- Array.uncons loc.locPath
    router <- capture path.head
    routeLoc (Location $ loc { locPath = path.tail }) router
  RQueryParam interpret -> do
    router <- interpret (QueryPairs loc.locQuery)
    routeLoc (Location $ loc { locQuery = [] }) router 
  RPathComponent segment router -> do 
    path <- Array.uncons loc.locPath
    guard $ path.head == segment
    routeLoc (Location $ loc { locPath = path.tail }) router
  RView a ->
    guard (loc.locPath == [] || loc.locPath == [""]) $> a

--------------------------------------------------------------------------------
-- Runners
--------------------------------------------------------------------------------

runRouteLoc 
  :: forall layout page handler
   . HasRouter layout page handler
  => Location 
  -> Routable layout 
  -> handler 
  -> Either RoutingError page
runRouteLoc loc nested handler = note FailedMatch $ 
  routeLoc loc $ mkRouter (RouteProxy :: RouteProxy layout) handler

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