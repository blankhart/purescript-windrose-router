module Windrose.Router ( module Export ) where

import Windrose.Router.API (
  kind Route, 
  RouteProxy(..), 
  type (:<|>), 
  type (:>), 
  P, C, M, Q, V
) as Export

import Windrose.Router.Error (
  RoutingError(..)
) as Export

import Windrose.Router.HasLinks (
  Link, 
  allLinks, 
  allLinksWith,
  class HasLinks
) as Export

import Windrose.Router.HasRouter (
  route,
  class HasRouter
) as Export

import Windrose.Router.Location (
  Location(..),
  class FromLocation,
  fromLocation,
  class ToLocation,
  toLocation
) as Export

import Windrose.Router.Routable (
  Routable, 
  mkRoutable,
  class Canonicalize
) as Export

import Windrose.Router.UrlPiece (
  class FromUrlPiece, 
  fromUrlPiece, 
  class ToUrlPiece, 
  toUrlPiece
) as Export
