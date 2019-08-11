module Servant.Routing.HasLinks where 

import Prelude

import Servant.API
import Servant.Routing.API
import Servant.Routing.IsEndpoint (class IsEndpoint)
import Servant.Routing.Routable (Routable)

import Data.Foldable (foldl)
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Heterogeneous.Folding (class FoldlRecord)
import Prim.Row as Row
import Prim.RowList (kind RowList, class RowToList)
import Record as Record

type Link = String

appendPathSegment :: String -> Link -> Link 
appendPathSegment s path = path <> "/" <> s

appendQueryString :: String -> Link -> Link 
appendQueryString s path = path <> "?" <> s

--------------------------------------------------------------------------------
-- allLinks
--------------------------------------------------------------------------------

allLinks
    :: forall layout links 
     . HasLinks layout Link links
    => Routable layout
    -> links
allLinks = allLinksWith identity

allLinksWith
    :: forall layout action links 
     . HasLinks layout action links
    => (Link -> action)
    -> Routable layout
    -> links
allLinksWith toAction _ = mkLinks (RouteProxy :: RouteProxy layout) toAction ""

--------------------------------------------------------------------------------
-- HasLinks
--------------------------------------------------------------------------------

class HasLinks (layout :: Route) (action :: Type) (links :: Type) | layout action -> links where
  mkLinks :: RouteProxy layout -> (Link -> action) -> Link -> links

-- | Path Alternative
instance hasLinksPathAltNil
  ::  ( HasLinks endpoint action link
      , IsEndpoint endpoint name
      , Row.Cons name link () links
      , Row.Lacks name ()
      , IsSymbol name
      )
  => HasLinks (endpoint :<|> NIL) action (Record links) where
  mkLinks _ toAction link = Record.insert (SProxy :: _ name)
    (mkLinks (RouteProxy :: _ endpoint) toAction link) 
    {}

else instance hasLinksPathAltCons
  ::  ( HasLinks endpoint action link
      , IsEndpoint endpoint name
      , Row.Cons name link sublinks links
      , Row.Lacks name sublinks
      , IsSymbol name
      , HasLinks sublayout action (Record sublinks)
      )
  => HasLinks (endpoint :<|> sublayout) action (Record links) where
  mkLinks _ toAction link = Record.insert (SProxy :: _ name) 
    (mkLinks (RouteProxy :: _ endpoint) toAction link) 
    (mkLinks (RouteProxy :: _ sublayout) toAction link)

-- | Path Component
else instance hasLinksPathComponent
  ::  ( HasLinks sublayout action links
      , IsSymbol s
      )
  => HasLinks (S s :> sublayout) action links where
  mkLinks _ toAction link = 
    mkLinks (RouteProxy :: _ sublayout) toAction 
      (appendPathSegment (reflectSymbol $ SProxy :: _ s) link)
 
-- | Capture
else instance hasLinksCapture
  ::  ( HasLinks sublayout action links
      , IsSymbol s
      , ToCapture a
      )
  => HasLinks (CAP s a :> sublayout) action (a -> links) where
  mkLinks _ toAction link = \a -> 
    mkLinks (RouteProxy :: _ sublayout) toAction 
      (appendPathSegment (toCapture a) link)

-- | CaptureMany
else instance hasLinksCaptureMany
  ::  ( HasLinks sublayout action links
      , IsSymbol s
      , ToCapture a
      )
  => HasLinks (CAPMANY s a :> sublayout) action (Array a -> links) where
  mkLinks _ toAction link = \captures -> 
    mkLinks (RouteProxy :: _ sublayout) toAction 
      (foldl (\l c -> appendPathSegment (toCapture c) l) link captures)

-- | QueryParam 
else instance hasLinksQueryParam 
  ::  ( HasLinks sublayout action links
      , RowToList params paramsRL
      , FoldlRecord QueryParamEntry (Array QueryParam) paramsRL params (Array QueryParam) 
      ) 
  => HasLinks (QPs params :> sublayout) action (Record params -> links) where 
  mkLinks _ toAction link = \params -> 
    mkLinks (RouteProxy :: _ sublayout) toAction
      (appendQueryString (formatQueryString (QueryParams params)) link)

-- | View
else instance hasLinksView
  ::  ( IsSymbol sym ) => HasLinks (VIEW sym) action action where 
  mkLinks _ toAction link = toAction link