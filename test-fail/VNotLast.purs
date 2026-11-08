-- expect: A view V must be the last combinator of an endpoint
module Test.Fail.VNotLast where

import Windrose.Router

type Api = V "a" :> P "b" :<|> V "c"

api = mkRoutable (RouteProxy :: RouteProxy Api)
