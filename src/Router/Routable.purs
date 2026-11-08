module Windrose.Router.Routable
  ( Routable
  , mkRoutable
  , class Canonicalize
  , class Flatten
  , class PrefixOnto
  , class Prefix
  ) where

import Prim.TypeError (class Fail, Above, Quote, Text)
import Type.Proxy (Proxy(..))
import Windrose.Router.API (type (:<|>), type (:>), C, M, P, Q, Route, RouteProxy, V)
import Windrose.Router.Canonical (Capture, CaptureMany, ECons, ENil, End, Endpoint, Endpoints, NoQuery, Path, Query)

-- | A token for the canonical form of a user-specified API.
newtype Routable (api :: Endpoints) = Routable (Proxy api)

-- | A smart constructor for the canonical form of the typelevel API
-- | specified by the user.
mkRoutable
  :: forall nested api
   . Canonicalize nested api
  => RouteProxy nested
  -> Routable api
mkRoutable _ = Routable (Proxy :: Proxy api)

-- | Determine the canonical form of an API: the list of its endpoints in
-- | the order they are declared, with shared prefixes distributed over the
-- | alternatives that follow them.
class Canonicalize (api :: Route) (canonical :: Endpoints) | api -> canonical

instance canonicalizeFlatten :: Flatten api ENil canonical => Canonicalize api canonical

type BadNestingError = Text "Invalid nesting in the definition of the API.  Endpoints must branch to the right.  Try adjusting parentheses."

-- | `Flatten r acc out` puts the endpoints of `r` in front of `acc`.
class Flatten (r :: Route) (acc :: Endpoints) (out :: Endpoints) | r acc -> out

instance flattenBadLeftAltSeq ::
  ( Fail (Above BadNestingError (Above (Text "Failure of type (a :<|> b) :> c in:") (Quote ((a :<|> b) :> c))))
  ) =>
  Flatten ((a :<|> b) :> c) acc out

else instance flattenBadLeftAltAlt ::
  ( Fail (Above BadNestingError (Above (Text "Failure of type (a :<|> b) :<|> c in:") (Quote ((a :<|> b) :<|> c))))
  ) =>
  Flatten ((a :<|> b) :<|> c) acc out

else instance flattenLeftSeq ::
  ( Flatten (a :> b :> c) acc out
  ) =>
  Flatten ((a :> b) :> c) acc out

else instance flattenAlt ::
  ( Flatten b acc acc'
  , Flatten a acc' out
  ) =>
  Flatten (a :<|> b) acc out

else instance flattenViewNotLast ::
  ( Fail (Above (Text "A view V must be the last combinator of an endpoint, in:") (Quote (V name :> rest)))
  ) =>
  Flatten (V name :> rest) acc out

else instance flattenSeq ::
  ( Flatten rest ENil endpoints
  , PrefixOnto (x :> rest) x endpoints acc out
  ) =>
  Flatten (x :> rest) acc out

else instance flattenView :: Flatten (V name) acc (ECons name (End NoQuery) acc)

else instance flattenNoView ::
  ( Fail (Above (Text "Every endpoint must end in a view V, but this does not:") (Quote r))
  ) =>
  Flatten r acc out

-- | `PrefixOnto ctx x endpoints acc out` puts `x` in front of each of the
-- | `endpoints`, and those in front of `acc`.  `ctx` is the fragment of the
-- | user's API being flattened, for error messages.
class PrefixOnto (ctx :: Route) (x :: Route) (endpoints :: Endpoints) (acc :: Endpoints) (out :: Endpoints) | ctx x endpoints acc -> out

instance prefixOntoNil :: PrefixOnto ctx x ENil acc acc

instance prefixOntoCons ::
  ( Prefix ctx x e e'
  , PrefixOnto ctx x rest acc out
  ) =>
  PrefixOnto ctx x (ECons name e rest) acc (ECons name e' out)

-- | `Prefix ctx x e e'` puts the path piece `x` in front of the endpoint `e`,
-- | rejecting a combinator that cannot appear there.  A view `V` never
-- | reaches here, since `Flatten` rejects `V name :> rest` first.
class Prefix (ctx :: Route) (x :: Route) (e :: Endpoint) (e' :: Endpoint) | ctx x e -> e'

instance prefixPath :: Prefix ctx (P s) e (Path s e)

else instance prefixCapture :: Prefix ctx (C s a) e (Capture s a e)

else instance prefixCaptureMany :: Prefix ctx (M s a) (End t) (CaptureMany s a t)

else instance prefixCaptureManyNotLast ::
  ( Fail (Above (Text "A capture-many M must be the last path piece of an endpoint, followed by at most a query Q and the view V, in:") (Quote ctx))
  ) =>
  Prefix ctx (M s a) e e'

else instance prefixQuery :: Prefix ctx (Q params) (End NoQuery) (End (Query params))

else instance prefixQueryNotLast ::
  ( Fail (Above (Text "A query Q must come immediately before the view V of an endpoint, in:") (Quote ctx))
  ) =>
  Prefix ctx (Q params) e e'
