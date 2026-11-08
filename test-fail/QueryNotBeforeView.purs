-- expect: A query Q must come immediately before the view V
module Test.Fail.QueryNotBeforeView where

import Windrose.Router

type Api = Q (q :: String) :> P "x" :> V "v"

api = mkRoutable (RouteProxy :: RouteProxy Api)
