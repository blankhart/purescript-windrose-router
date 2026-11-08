-- | The canonical form of a typelevel API.
-- |
-- | `Windrose.Router.Routable.Canonicalize` turns an API written in the user
-- | DSL of kind `Route` into an ordered list of named endpoints of kind
-- | `Endpoints`, which is what `HasRouter` and `HasLinks` interpret. The
-- | kinds encode the rules for a well-formed endpoint: path pieces come
-- | first, a capture-many is the last path piece, and an optional query sits
-- | immediately before the view.
-- |
-- | Users of the library do not write these types.  The module is exported
-- | only because instances in other modules mention them.
module Windrose.Router.Canonical
  ( Endpoints
  , ENil
  , ECons
  , Endpoint
  , Path
  , Capture
  , CaptureMany
  , End
  , Terminal
  , NoQuery
  , Query
  ) where

-- | Kind for a canonical API: an ordered list of endpoints, each labelled
-- | with the name of its view.
data Endpoints

foreign import data ENil :: Endpoints

foreign import data ECons :: Symbol -> Endpoint -> Endpoints -> Endpoints

-- | Kind for a single endpoint: path pieces, then a terminal.
data Endpoint

-- | A literal path segment, from `P`.
foreign import data Path :: Symbol -> Endpoint -> Endpoint

-- | A single captured segment, from `C`.
foreign import data Capture :: Symbol -> Type -> Endpoint -> Endpoint

-- | The rest of the path, from `M`.  Nothing but a terminal can follow it.
foreign import data CaptureMany :: Symbol -> Type -> Terminal -> Endpoint

-- | The end of the path.
foreign import data End :: Terminal -> Endpoint

-- | Kind for what follows the path of an endpoint.
data Terminal

-- | No query string.
foreign import data NoQuery :: Terminal

-- | A query string, from `Q`.
foreign import data Query :: Row Type -> Terminal
