module Windrose.Router.HasLinks
  ( Link
  , allLinks
  , allLinksWith
  , class HasLinks
  , mkLinks
  , class EndpointLink
  , endpointLink
  , class TerminalLink
  , terminalLink
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldlRecord)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record as Record
import Type.Proxy (Proxy(..))
import Windrose.Router.Canonical (Capture, CaptureMany, ECons, ENil, End, Endpoint, Endpoints, NoQuery, Path, Query, Terminal)
import Windrose.Router.PercentEncoding (percentEncode)
import Windrose.Router.QueryPairs (FoldToQueryPairs, QueryPairs(..), QueryParams(..), toQueryPairs)
import Windrose.Router.Routable (Routable)
import Windrose.Router.UrlPiece (class ToUrlPiece, toUrlPiece)

type Link = String

-- | Path segments, including `P` literals, are percent-encoded so that a
-- | value such as `"a/b?c"` stays one segment.
appendPathSegment :: String -> Link -> Link
appendPathSegment s path = path <> "/" <> percentEncode s

-- | The root has no path segments to print, and is "/".
rooted :: Link -> Link
rooted path = if path == "" then "/" else path

-- | A query with no pairs, as when every parameter is `Nothing` or an
-- | empty array, adds nothing to the link.
appendQueryString :: QueryPairs -> Link -> Link
appendQueryString (QueryPairs qps) path = case qps of
  [] -> path
  _ -> rooted path <> "?" <> joinWith "&" (map (\(Tuple k v) -> percentEncode k <> "=" <> percentEncode v) qps)

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
allLinksWith toAction _ = mkLinks (Proxy :: _ layout) toAction

--------------------------------------------------------------------------------
-- HasLinks
--------------------------------------------------------------------------------

-- | `HasLinks` instances build the record of link generators for a
-- | canonical `layout`, with one field per endpoint.
class HasLinks (layout :: Endpoints) (action :: Type) (links :: Type) | layout action -> links where
  mkLinks :: Proxy layout -> (Link -> action) -> links

instance hasLinksNil :: HasLinks ENil action (Record ()) where
  mkLinks _ _ = {}

instance hasLinksCons ::
  ( EndpointLink endpoint action link
  , HasLinks rest action (Record sublinks)
  , Row.Cons name link sublinks links
  , Row.Lacks name sublinks
  , IsSymbol name
  ) =>
  HasLinks (ECons name endpoint rest) action (Record links) where
  mkLinks _ toAction = Record.insert (Proxy :: _ name)
    (endpointLink (Proxy :: _ endpoint) toAction "")
    (mkLinks (Proxy :: _ rest) toAction)

-- | `EndpointLink` instances accumulate the path of an endpoint, left to
-- | right, into a `Link`.
class EndpointLink (endpoint :: Endpoint) (action :: Type) (link :: Type) | endpoint action -> link where
  endpointLink :: Proxy endpoint -> (Link -> action) -> Link -> link

instance endpointLinkPath ::
  ( EndpointLink rest action link
  , IsSymbol s
  ) =>
  EndpointLink (Path s rest) action link where
  endpointLink _ toAction link =
    endpointLink (Proxy :: _ rest) toAction
      (appendPathSegment (reflectSymbol (Proxy :: _ s)) link)

instance endpointLinkCapture ::
  ( EndpointLink rest action link
  , ToUrlPiece a
  ) =>
  EndpointLink (Capture s a rest) action (a -> link) where
  endpointLink _ toAction link = \a ->
    endpointLink (Proxy :: _ rest) toAction
      (appendPathSegment (toUrlPiece a) link)

instance endpointLinkCaptureMany ::
  ( TerminalLink terminal action link
  , ToUrlPiece a
  ) =>
  EndpointLink (CaptureMany s a terminal) action (Array a -> link) where
  endpointLink _ toAction link = \captures ->
    terminalLink (Proxy :: _ terminal) toAction
      (foldl (\l c -> appendPathSegment (toUrlPiece c) l) link captures)

instance endpointLinkEnd ::
  ( TerminalLink terminal action link
  ) =>
  EndpointLink (End terminal) action link where
  endpointLink _ = terminalLink (Proxy :: _ terminal)

-- | `TerminalLink` instances finish a `Link` once its path is complete.
class TerminalLink (terminal :: Terminal) (action :: Type) (link :: Type) | terminal action -> link where
  terminalLink :: Proxy terminal -> (Link -> action) -> Link -> link

instance terminalLinkNoQuery :: TerminalLink NoQuery action action where
  terminalLink _ toAction link = toAction (rooted link)

instance terminalLinkQuery ::
  ( RowToList params paramsRL
  , FoldlRecord FoldToQueryPairs QueryPairs paramsRL params QueryPairs
  ) =>
  TerminalLink (Query params) action (Record params -> action) where
  terminalLink _ toAction link = \params ->
    toAction (rooted (appendQueryString (toQueryPairs (QueryParams params)) link))
