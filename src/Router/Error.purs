module Windrose.Router.Error (
  RoutingError(..)
) where

import Prelude (class Eq, class Show)

data RoutingError = FailedMatch | FailedParse

derive instance eqRoutingError :: Eq RoutingError

instance showRoutingError :: Show RoutingError where 
  show = case _ of 
    FailedMatch -> "FailedMatch"
    FailedParse -> "FailedParse"