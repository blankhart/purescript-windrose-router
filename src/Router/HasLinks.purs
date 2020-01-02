module Windrose.Router.HasLinks (
  Link, 
  allLinks, 
  allLinksWith, 
  class HasLinks, 
  mkLinks
) where 

import Prelude

import Windrose.Router.API (type (:<|>), type (:>), C, M, NIL, P, Q, RouteProxy(..), V, kind Route)
import Windrose.Router.IsEndpoint (class IsEndpoint)
import Windrose.Router.QueryPairs (FoldToQueryPairs, QueryPairs(..), QueryParams(..), toQueryPairs)
import Windrose.Router.Routable (Routable)
import Windrose.Router.UrlPiece (class ToUrlPiece, toUrlPiece)

import Data.Foldable (foldl)
import Data.String (joinWith)
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldlRecord)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record as Record

type Link = String

appendPathSegment :: String -> Link -> Link 
appendPathSegment s path = path <> "/" <> s

appendQueryString :: QueryPairs -> Link -> Link 
appendQueryString (QueryPairs qps) path = 
  let string = joinWith "&" $ map (\(Tuple k v) -> k <> "=" <> v) qps 
  in path <> "?" <> string 
  

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

instance hasLinksRouteSumNil
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

else instance hasLinksRouteSumCons
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

else instance hasLinksPathComponent
  ::  ( HasLinks sublayout action links
      , IsSymbol s
      )
  => HasLinks (P s :> sublayout) action links where
  mkLinks _ toAction link = 
    mkLinks (RouteProxy :: _ sublayout) toAction 
      (appendPathSegment (reflectSymbol (SProxy :: SProxy s)) link)
 
else instance hasLinksCapture
  ::  ( HasLinks sublayout action links
      , IsSymbol s
      , ToUrlPiece a
      )
  => HasLinks (C s a :> sublayout) action (a -> links) where
  mkLinks _ toAction link = \a -> 
    mkLinks (RouteProxy :: _ sublayout) toAction 
      (appendPathSegment (toUrlPiece a) link)

else instance hasLinksCaptureMany
  ::  ( HasLinks sublayout action links
      , IsSymbol s
      , ToUrlPiece a
      )
  => HasLinks (M s a :> sublayout) action (Array a -> links) where
  mkLinks _ toAction link = \captures -> 
    mkLinks (RouteProxy :: _ sublayout) toAction 
      (foldl (\l c -> appendPathSegment (toUrlPiece c) l) link captures)

else instance hasLinksQueryString
  ::  ( HasLinks sublayout action links
      , RowToList params paramsRL
      , FoldlRecord FoldToQueryPairs QueryPairs paramsRL params QueryPairs
      ) 
  => HasLinks (Q params :> sublayout) action (Record params -> links) where 
  mkLinks _ toAction link = \params -> 
    mkLinks (RouteProxy :: _ sublayout) toAction
      (appendQueryString (toQueryPairs (QueryParams params)) link)

else instance hasLinksView
  ::  ( IsSymbol sym ) => HasLinks (V sym) action action where 
  mkLinks _ toAction link = toAction link