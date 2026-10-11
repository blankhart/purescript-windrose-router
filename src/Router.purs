module Windrose.Router (module Export) where

import Windrose.Router.API
  ( type (:<|>)
  , type (:>)
  , C
  , M
  , P
  , Q
  , Route
  , RouteProxy(..)
  , V
  ) as Export
import Windrose.Router.Error
  ( RoutingError(..)
  ) as Export
import Windrose.Router.HasLinks
  ( class HasLinks
  , Link
  , allLinks
  , allLinksWith
  ) as Export
import Windrose.Router.HasRouter
  ( class HasRouter
  , route
  ) as Export
import Windrose.Router.Location
  ( class FromLocation
  , class ToLocation
  , Location(..)
  , fromLocation
  , toLocation
  ) as Export
import Windrose.Router.Routable
  ( class Canonicalize
  , Routable
  , mkRoutable
  ) as Export
import Windrose.Router.UrlPiece
  ( class FromUrlPiece
  , class ToUrlPiece
  , fromUrlPiece
  , toUrlPiece
  ) as Export
